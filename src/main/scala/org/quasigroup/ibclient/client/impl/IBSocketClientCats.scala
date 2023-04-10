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

  private val msgQueueDeferred = Deferred.unsafe[F, Queue[F, ResponseMsg]]
  private val msgPullingFiberDeferred =
    Deferred.unsafe[F, Fiber[F, Throwable, Unit]]
  private val _clientId = Deferred.unsafe[F, Int]
  private val _serverVersion = Deferred.unsafe[F, Int]

  private def startMsgConsumption: F[Unit] = {
    for {
      msgQueue <- Queue.unbounded[F, ResponseMsg]
      pullFiber <- socket.reads
        .through(ibFramesString.toPipeByte)
        .evalTap(strs => Console[F].println(strs.mkString("[", ",", "]")))
        .evalMap(Decoder.decodeMsg(_).liftTo[F])
        .evalTap(Console[F].println)
        .takeWhile(_ != ConnectionClosed)
        .evalTap(msgQueue.offer)
        .compile
        .drain
        .start
      _ <- msgQueueDeferred.complete(msgQueue)
      _ <- msgPullingFiberDeferred.complete(pullFiber)
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

  private def requestBeforeAPIStart(chunks: Array[Byte]): F[Array[String]] =
    for {
      _ <- socket.write(Chunk.array(chunks))
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
    } yield result

  private def fetchSingleResponse[Req: Encoder, Resp <: ResponseMsg: ClassTag](
      request: Req
  ): F[Resp] = for {
    _ <- socket.write(Chunk.array(encode[Req](request)))
    msgQueue <- msgQueueDeferred.get
    resp <- Stream
      .fromQueueUnterminated(msgQueue)
      .collectFirst { case item: Resp => item }
      .compile
      .lastOrError
  } yield resp

  override def reqCurrentTime(): F[CurrentTime] =
    fetchSingleResponse[ReqCurrentTime, CurrentTime](ReqCurrentTime())
  override def eConnect(clientId: Int): F[ConnectionAck] =
    for {
      rawResponse <- requestBeforeAPIStart(
        "API\u0000".getBytes ++ encode[String](
          buildVersionString(MIN_VERSION, MAX_VERSION)
        )
      )
      resp <- Decoder.decode[ConnectionAck](rawResponse).liftTo[F]
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
      _ <- requestBeforeAPIStart(
        encode[StartAPI](
          StartAPI(
            clientId = clientId,
            optionalCapabilities = optionalCapabilities.getOrElse("")
          )
        )
      )
      _ <- Console[F].println("Start msg pulling")
      _ <- startMsgConsumption
    } yield ()

  }

  override def reqFamilyCodes(): F[FamilyCodes] =
    fetchSingleResponse[ReqFamilyCodes, FamilyCodes](ReqFamilyCodes())
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
