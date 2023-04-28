package org.quasigroup.ibclient.impl

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.IBClient.*
import org.quasigroup.ibclient.encoder.Encoder
import org.quasigroup.ibclient.encoder.Encoder.{*, given}
import org.quasigroup.ibclient.decoder.Decoder
import org.quasigroup.ibclient.exceptions.InvalidMessageLengthException
import org.quasigroup.ibclient.request.{MsgEncoders, RequestMsg}
import org.quasigroup.ibclient.request.RequestMsg.*
import org.quasigroup.ibclient.response.ResponseMsg
import org.quasigroup.ibclient.response.ResponseMsg.*
import org.quasigroup.ibclient.response.MsgDecoders.given
import cats.effect.*
import cats.effect.std.{Backpressure, Console, Mutex, Queue}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.*
import fs2.concurrent.*
import fs2.interop.scodec.StreamDecoder
import fs2.io.net.*
import scodec.*
import scodec.Err.General
import scodec.bits.*
import scodec.codecs.*

import scala.concurrent.duration.DurationInt
import scala.reflect.ClassTag
import org.quasigroup.ibclient.response.MsgDecoders

class IBSocketClientCats[F[_]: Async: Console](
    socket: Socket[F],
    optionalCapabilities: Option[String]
) extends IBClient[F]:

  import IBSocketClientCats.{*, given}

  private val requestRateLimiterRef: Ref[F, Int] = Ref.unsafe(50)
  private val msgPushingFiberDeferred =
    Deferred.unsafe[F, Fiber[F, Throwable, Unit]]
  private val msgTopicDeferred = Deferred.unsafe[F, Topic[F, ResponseMsg]]
  private val msgPullingFiberDeferred =
    Deferred.unsafe[F, Fiber[F, Throwable, Unit]]
  private val _clientId = Deferred.unsafe[F, Int]
  private val _serverVersion = Deferred.unsafe[F, IBClient.ServerVersion]

  private def startMsgConsumption: F[Unit] =
    for
      given ServerVersion <- _serverVersion.get
      msgQueue <- Queue.unbounded[F, ResponseMsg]
      msgTopic <- Topic[F, ResponseMsg]
      pushFiber <-
        Stream
          .awakeEvery(1.second)
          .evalTap(_ => requestRateLimiterRef.set(50))
          .compile
          .drain
          .start
      pullFiber <- Stream
        .fromQueueUnterminated(msgQueue)
        .concurrently(
          socket.reads
            .through(ibFramesString.toPipeByte)
            .evalTap(strs => Console[F].println(strs.mkString("[", ",", "]")))
            .evalMap(MsgDecoders.decode(_).liftTo[F])
            .evalTap(Console[F].println)
            .takeThrough(_ != ConnectionClosed)
            .evalTap(msgQueue.offer)
        )
        .through(msgTopic.publish)
        .compile
        .drain
        .start
      _ <- msgPullingFiberDeferred.complete(pullFiber)
      _ <- msgPushingFiberDeferred.complete(pushFiber)
      _ <- msgTopicDeferred.complete(msgTopic)
    yield ()

  private val ibFramesString: StreamDecoder[Array[String]] =
    for
      msgSize <- StreamDecoder.many(int32)
      rawMessage <-
        if msgSize <= MAX_MSG_LENGTH then
          StreamDecoder.once(
            bytes(msgSize).map(_.decodeUtf8Lenient.split(0.toChar))
          )
        else
          StreamDecoder.raiseError(
            InvalidMessageLengthException(
              "message size is too large: " + msgSize
            )
          )
    yield rawMessage

  private def fetchSingleResponse[
      Req <: RequestMsg: Encoder,
      Resp <: ResponseMsg: ClassTag
  ](
      request: Req
  ): F[Resp] =
    for
      _ <- requestOnly(request)
      msgStream <- msgTopicDeferred.get
      resp <- msgStream.subscribeUnbounded
        .collectFirst { case item: Resp => item }
        .evalTap(item => Console[F].println(s"fetched msg:$item"))
        .compile
        .lastOrError
    yield resp

  private def fetchResponses[
      Req <: RequestMsg: Encoder,
      Resp <: ResponseMsg: ClassTag
  ](
      request: Req
  ): Stream[F, Resp] =
    for
      _ <- Stream.eval(requestOnly(request))
      topic <- Stream.eval(msgTopicDeferred.get)
      position <- topic.subscribeUnbounded
        .dropWhile {
          case item: Resp => false
          case _          => true
        }
        .collectWhile { case item: Resp => item }
        .evalTap(item => Console[F].println(s"fetched msg:$item"))
    yield position

  def requestOnly[Req <: RequestMsg: Encoder](
      request: Req
  ): F[Unit] =
    for
      given ServerVersion <- _serverVersion.get
      bytes <- MsgEncoders.encode[F, Req](request)
      _ <- requestRateLimiterRef.get.iterateUntil(_ > 0)
      _ <- requestRateLimiterRef.update(_ - 1)
      _ <- socket.write(Chunk.array(bytes))
    yield ()

  private def eConnect(clientId: Int): F[ConnectionAck] =
    for
      encoded <- ("API\u0000".getBytes ++ MsgEncoders.unsafeEncode[String](
        buildVersionString(MIN_VERSION, MAX_VERSION)
      )).pure
      _ <- socket.write(Chunk.array(encoded))
      sizeRaw <- socket.read(4)
      resultRaw <- sizeRaw
        .map(_.toByteVector.toInt())
        .flatTraverse[F, Chunk[Byte]](msgSize =>
          if msgSize <= MAX_MSG_LENGTH then socket.read(msgSize)
          else
            Async[F].raiseError(
              new InvalidMessageLengthException(
                "message size is too large: " + msgSize
              )
            )
        )
      result <- resultRaw
        .map(_.toByteVector.decodeUtf8Lenient.split(0.toChar))
        .liftTo[F](
          new InvalidMessageLengthException("message empty")
        )
      resp <- summon[Decoder[ConnectionAck]](result).liftTo[F]
      _ <- _clientId.complete(clientId)
      _ <- _serverVersion.complete(resp.serverVersion)
      _ <- startAPI
    yield resp

  private def eDisconnect: F[Unit] =
    for
      _ <- msgPushingFiberDeferred.get.flatMap(_.cancel)
      _ <- msgPullingFiberDeferred.get.flatMap(_.cancel)
      _ <- Console[F].println("Msg pull stopped")
    yield ()

  private def startAPI: F[Unit] =
    for
      clientId <- _clientId.get
      _ <- requestOnly[StartAPI](
        StartAPI(
          clientId = clientId,
          optionalCapabilities = optionalCapabilities.getOrElse("")
        )
      )
      _ <- Console[F].println("Start msg pulling")
      _ <- startMsgConsumption
    yield ()

  override def reqFamilyCodes: F[FamilyCodes] =
    fetchSingleResponse[ReqFamilyCodes, FamilyCodes](ReqFamilyCodes())

  override def reqCurrentTime: F[CurrentTime] =
    fetchSingleResponse[ReqCurrentTime, CurrentTime](ReqCurrentTime())

  override def reqScannerParameters: F[ScannerParameters] =
    fetchSingleResponse[ReqScannerParameters, ScannerParameters](
      ReqScannerParameters()
    )

  override def setServerLogLevel(level: Int): F[Unit] =
    requestOnly[SetServerLogLevel](
      SetServerLogLevel(loglevel = level)
    )

  override def reqPositions: Stream[F, PositionMsg] =
    fetchResponses[ReqPositions, PositionMsg](ReqPositions())

  override def cancelPositions: F[Unit] =
    requestOnly[CancelPositions](
      CancelPositions()
    )

  override def reqManagedAccts: F[ManagedAccounts] =
    fetchSingleResponse[ReqManagedAccts, ManagedAccounts](ReqManagedAccts())

  override def requestFA(faDataType: Int): F[ReceiveFA] =
    fetchSingleResponse[RequestFA, ReceiveFA](
      RequestFA(faDataType = faDataType)
    )

end IBSocketClientCats

object IBSocketClientCats:
  final case class ConnectionAck(
      serverVersion: IBClient.ServerVersion,
      time: String
  )

  inline given Decoder[IBClient.ServerVersion] =
    summon[Decoder[Int]].map(IBClient.ServerVersion.apply)

  val MAX_MSG_LENGTH: Int = 0xffffff
  def make[F[_]: Async: Console](
      host: Host = host"127.0.0.1",
      port: Port = port"7496",
      optionalCapabilities: Option[String] = None,
      clientId: Int = 10
  ): Resource[F, IBClient[F]] =
    for
      client <- Network[F]
        .client(SocketAddress(host, port))
        .map(new IBSocketClientCats[F](_, optionalCapabilities))
        .evalTap(_.eConnect(clientId))
      _ <- Resource.onFinalize(client.eDisconnect)
    yield client

end IBSocketClientCats
