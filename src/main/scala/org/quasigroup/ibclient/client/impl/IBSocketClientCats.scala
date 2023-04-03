package org.quasigroup.ibclient.client.impl

import cats.effect.{Async, MonadCancel, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.*
import fs2.interop.scodec.StreamDecoder
import fs2.io.net.*
import org.quasigroup.ibclient.client.IBClient
import org.quasigroup.ibclient.client.IBClient.*
import org.quasigroup.ibclient.client.decoder.Decoder
import org.quasigroup.ibclient.client.encoder.Encoder
import org.quasigroup.ibclient.client.encoder.Encoder.given
import org.quasigroup.ibclient.client.exceptions.InvalidMessageLengthException
import org.quasigroup.ibclient.client.types.ConnectionAck
import scodec.bits.*
import scodec.codecs.*

class IBSocketClientCats[F[_]: Async](
    socket: Socket[F],
    optionalCapabilities: Option[String]
) extends IBClient[F] {

  import IBSocketClientCats.{*, given}

  private val ibFramesString: StreamDecoder[String] =
    StreamDecoder
      .many(bytes(4))
      .flatMap(sizeInByte =>
        val msgSize = sizeInByte.toInt(false)
        if msgSize <= MAX_MSG_LENGTH then StreamDecoder.once(utf8)
        else
          StreamDecoder.raiseError(
            InvalidMessageLengthException("message is too long: " + msgSize)
          )
      )

  private def encode[T: Encoder](raw: T): Array[Byte] = {
    val encoded = summon[Encoder[T]].apply(raw)
    val length = Encoder.Length(encoded.length)
    val lengthEncoded = Encoder.LengthEncoder.apply(length)
    (lengthEncoded ++ encoded).toArray
  }

  private def request(chunks: Array[Byte]): F[Option[Array[String]]] = for {
    _ <- socket.write(Chunk.array(chunks))
    result <- socket.reads.through(ibFramesString.toPipeByte).compile.last
  } yield {
    result.map(_.split("\u0000"))
  }

  private def requestStream(chunks: Array[Byte]): fs2.Stream[F, Array[String]] =
    for {
      _ <- Stream.eval(socket.write(Chunk.array(chunks)))
      result <- socket.reads.through(ibFramesString.toPipeByte)
    } yield result.split("\u0000")

  override def eConnect(clientId: Int): F[ConnectionAck] =
    for {
      rawResponse <- request(
        "API\u0000".getBytes ++ encode(
          buildVersionString(MIN_VERSION, MAX_VERSION)
        )
      )
      resp <- rawResponse
        .toRight(RuntimeException("no response"))
        .flatMap(Decoder.decode[ConnectionAck])
        .liftTo[F]
    } yield resp

  override def eDisconnect(resetState: Boolean): F[Unit] = Async[F].unit
}

object IBSocketClientCats {
  given Decoder[ConnectionAck] with
    def apply(entry: Array[String]): Either[Throwable, ConnectionAck] = {
      entry match {
        case Array(version, timestamp) =>
          Right(ConnectionAck(version.toInt, timestamp))
        case _ => Left(RuntimeException("wrong response"))
      }
    }

  val MAX_MSG_LENGTH: Int = 0xffffff
  def make[F[_]: Async](
      host: Host = host"127.0.0.1",
      port: Port = port"7496",
      optionalCapabilities: Option[String] = None
  ): Resource[F, IBClient[F]] =
    Network[F]
      .client(SocketAddress(host, port))
      .map(new IBSocketClientCats[F](_, optionalCapabilities))
}
