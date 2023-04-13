package org.quasigroup.ibclient.client.impl

import cats.Applicative
import cats.effect.*
import cats.effect.std.{Console, Queue}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.*
import fs2.concurrent.*
import fs2.interop.scodec.StreamDecoder
import fs2.io.net.*
import org.quasigroup.ibclient.client.IBClient
import org.quasigroup.ibclient.client.IBClient.*
import org.quasigroup.ibclient.client.decoder.Decoder
import org.quasigroup.ibclient.client.encoder.Encoder
import org.quasigroup.ibclient.client.encoder.Encoder.{*, given}
import org.quasigroup.ibclient.client.exceptions.InvalidMessageLengthException
import org.quasigroup.ibclient.client.request.RequestMsg.*
import org.quasigroup.ibclient.client.response.ResponseMsg
import org.quasigroup.ibclient.client.response.ResponseMsg.*
import org.quasigroup.ibclient.client.types.ConnectionAck
import scodec.Err.General
import scodec.bits.*
import scodec.codecs.*

import scala.reflect.ClassTag

class IBSocketClientCats[F[_]: Async: Console](
    socket: Socket[F],
    optionalCapabilities: Option[String]
) extends IBClient[F] {

  import IBSocketClientCats.{*, given}

  private val msgTopicDeferred = Deferred.unsafe[F, Topic[F, ResponseMsg]]
  private val msgPullingFiberDeferred =
    Deferred.unsafe[F, Fiber[F, Throwable, Unit]]
  private val _clientId = Deferred.unsafe[F, Int]
  private val _serverVersion = Deferred.unsafe[F, Int]

  private def startMsgConsumption: F[Unit] = {
    for {
      msgQueue <- Queue.unbounded[F, ResponseMsg]
      msgTopic <- Topic[F, ResponseMsg]
      pullFiber <- Stream
        .fromQueueUnterminated(msgQueue)
        .concurrently(
          socket.reads
            .through(ibFramesString.toPipeByte)
            .evalTap(strs => Console[F].println(strs.mkString("[", ",", "]")))
            .evalMap(Decoder.decodeMsg(_).liftTo[F])
            .evalTap(Console[F].println)
            .takeWhile(_ != ConnectionClosed)
            .evalTap(msgQueue.offer)
        )
        .through(msgTopic.publish)
        .compile
        .drain
        .start
      _ <- msgPullingFiberDeferred.complete(pullFiber)
      _ <- msgTopicDeferred.complete(msgTopic)
    } yield ()
  }

  private val ibFramesString: StreamDecoder[Array[String]] =
    StreamDecoder
      .many(int32)
      .flatMap(msgSize =>
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
      )

  private def fetchSingleResponse[Req: Encoder, Resp <: ResponseMsg: ClassTag](
      request: Req
  ): F[Resp] = for {
    _ <- socket.write(Chunk.array(encode[Req](request)))
    msgStream <- msgTopicDeferred.get
    resp <- msgStream.subscribeUnbounded
      .collectFirst { case item: Resp => item }
      .evalTap(item => Console[F].println(s"fetched msg:$item"))
      .compile
      .lastOrError
  } yield resp

  private def fetchResponsesWithEndType[
      Req: Encoder,
      Resp <: ResponseMsg: ClassTag,
      RespEnd <: ResponseMsg: ClassTag
  ](
      request: Req,
      endInstance: RespEnd
  ): Stream[F, Resp] =
    Stream.eval(socket.write(Chunk.array(encode[Req](request)))) >> Stream
      .eval(msgTopicDeferred.get)
      .flatMap(_.subscribeUnbounded)
      .through(
        _.filter {
          case item: Resp       => true
          case endItem: RespEnd => true
          case _                => false
        }
          .takeWhile(_ != endInstance)
          .map(_.asInstanceOf[Resp])
          .evalTap(item => Console[F].println(s"fetched msg:$item"))
      )

  private def fireAndForget[Req: Encoder](
      request: Req
  ): F[Unit] = for {
    requestEncoded <- encode[Req](request).pure
    _ <- socket.write(Chunk.array(requestEncoded))
  } yield ()

  override def eConnect(clientId: Int): F[ConnectionAck] =
    for {
      encoded <- ("API\u0000".getBytes ++ encode[String](
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
      resp <- Decoder.decode[ConnectionAck](result).liftTo[F]
      _ <- _clientId.complete(clientId)
      _ <- _serverVersion.complete(resp.serverVersion)
      _ <- startAPI
    } yield resp

  override def eDisconnect(): F[Unit] =
    for {
      _ <- msgPullingFiberDeferred.get.flatMap(_.cancel)
      _ <- Console[F].println("Msg pull stopped")
    } yield ()

  private def startAPI: F[Unit] = {
    for {
      clientId <- _clientId.get
      encoded <- encode[StartAPI](
        StartAPI(
          clientId = clientId,
          optionalCapabilities = optionalCapabilities.getOrElse("")
        )
      ).pure
      _ <- socket.write(Chunk.array(encoded))
      _ <- Console[F].println("Start msg pulling")
      _ <- startMsgConsumption
    } yield ()

  }

  override def reqFamilyCodes(): F[FamilyCodes] =
    fetchSingleResponse[ReqFamilyCodes, FamilyCodes](ReqFamilyCodes())

  override def reqCurrentTime(): F[CurrentTime] =
    fetchSingleResponse[ReqCurrentTime, CurrentTime](ReqCurrentTime())

  override def reqScannerParameters(): F[ScannerParameters] =
    fetchSingleResponse[ReqScannerParameters, ScannerParameters](
      ReqScannerParameters()
    )

  override def setServerLogLevel(level: Int): F[Unit] =
    fireAndForget[SetServerLogLevel](
      SetServerLogLevel(loglevel = level)
    )

  override def reqPositions(): Stream[F, Position] =
    fetchResponsesWithEndType[ReqPositions, Position, PositionEnd.type](
      ReqPositions(),
      PositionEnd
    )

  override def cancelPositions(): F[Unit] =
    fireAndForget[CancelPositions](
      CancelPositions()
    )

  override def reqManagedAccts(): F[ManagedAccounts] =
    fetchSingleResponse[ReqManagedAccts, ManagedAccounts](ReqManagedAccts())

  override def requestFA(faDataType: Int): F[ReceiveFA] =
    fetchSingleResponse[RequestFA, ReceiveFA](
      RequestFA(faDataType = faDataType)
    )

  //  override def reqNewsBulletins(allMsgs: Boolean): F[UpdateNewsBulletin] =
//    fetchSingleResponse[ReqNewsBulletins, UpdateNewsBulletin](
//      ReqNewsBulletins(allMsgs = allMsgs)
//    )
}

object IBSocketClientCats {
  given Decoder[ConnectionAck] with
    override def apply(
        entry: Array[String]
    ): Either[Throwable, ConnectionAck] = {
      entry match {
        case Array(version, timestamp) =>
          Right(ConnectionAck(version.toInt, timestamp))
        case _ => Left(RuntimeException("wrong response"))
      }
    }

  end given

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
      _ <- Resource.onFinalize(client.eDisconnect())
    yield client
}
