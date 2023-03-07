package org.quasigroup

import fs2.{Chunk, Stream, text}
import cats.effect.Async
import cats.effect.std.Console
import fs2.io.net.Network
import cats.syntax.all._
import com.comcast.ip4s._
import org.quasigroup.EMessage.EMPTY_LENGTH_HEADER
object IBClient {
  val MIN_VERSION = 100 // envelope encoding, applicable to useV100Plus mode only
  val MIN_SERVER_VER_BOND_ISSUERID = 176
  val MAX_VERSION: Int = MIN_SERVER_VER_BOND_ISSUERID // ditto

  def buildVersionString(minVersion: Int, maxVersion: Int) = "v" + (if (minVersion < maxVersion) minVersion + ".." + maxVersion else minVersion)

  def twsClient[F[_]: Async: Console: Network](host: Host = host"127.0.0.1", port: Port = port"8080"):  Stream[F, Unit] =
    Stream.resource(Network[F].client(SocketAddress(host, port))).flatMap { socket =>
        Stream.eval(socket.write(Chunk.array("API\0".getBytes ++ EMPTY_LENGTH_HEADER ++ buildVersionString(MIN_VERSION, MAX_VERSION).getBytes))) ++
          socket.reads
          .through(text.utf8.decode)
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
