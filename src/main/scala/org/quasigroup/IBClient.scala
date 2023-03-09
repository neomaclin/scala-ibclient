package org.quasigroup

import fs2.{Chunk, Stream, text}
import cats.effect.Async
import cats.effect.std.Console
import fs2.io.net.Network
import scodec.bits._
import scodec.codecs._
import fs2.interop.scodec._
import cats.syntax.all._
import com.comcast.ip4s._
import org.quasigroup.EMessage.EMPTY_LENGTH_HEADER


object IBClient {
  val MIN_VERSION = 100 // envelope encoding, applicable to useV100Plus mode only
  val MIN_SERVER_VER_BOND_ISSUERID = 176
  val MAX_VERSION: Int = MIN_SERVER_VER_BOND_ISSUERID // ditto

  def buildVersionString(minVersion: Int, maxVersion: Int) = "v" + (if (minVersion < maxVersion) minVersion + ".." + maxVersion else minVersion)

  def sizeOfBuildVersion(minVersion: Int, maxVersion: Int) = buildVersionString(minVersion, maxVersion).getBytes.length

  private def lengthToChunkBytes(length: Int): Chunk[Byte] = {
    Chunk.array(Array((0xff & (length >> 24)).toByte,
      (0xff & (length >> 16)).toByte,
      (0xff & (length >> 8)).toByte,
      (0xff & length).toByte))
  }

  private def bytesToLength(bytes: ByteVector): Int = {

    (((bytes(0) << 24) + (bytes(1) << 16) + (bytes(2) << 8) + (bytes(3) << 0)) & 0xffffffffL).toInt
  }


  val ibFramesString: StreamDecoder[String] =
    StreamDecoder.many(bytes(4)).flatMap(sizeInByte => StreamDecoder.once(utf8))


  def twsClient[F[_] : Async : Console : Network](host: Host = host"127.0.0.1", port: Port = port"7496"): Stream[F, Unit] =
    Stream.resource(Network[F].client(SocketAddress(host, port))).flatMap { socket =>
      Stream.eval(socket.write(Chunk.array("API\u0000".getBytes) ++ lengthToChunkBytes(sizeOfBuildVersion(MIN_VERSION, MAX_VERSION)) ++ Chunk.array(buildVersionString(MIN_VERSION, MAX_VERSION).getBytes))) ++
        socket.reads.through(ibFramesString.toPipeByte).flatMap(str => Stream(str.split('\u0000'):_*))
          .foreach { response =>
            Console[F].println(s"Response: $response")
          }
    }

  //  def FIXClient[F[_] : Async : Console : Network](host: Host = host"127.0.0.1", port: Port = port"8080"): Stream[F, Unit] =
  //    Stream.resource(Network[F].client(SocketAddress(host, port))).flatMap { socket =>
  //      Stream.chunk()
  //        .through(socket.writes) ++
  //        socket.reads
  //          .through(text.utf8.decode)
  //          .foreach { response =>
  //            Console[F].println(s"Response: $response")
  //          }
  //    }
}
