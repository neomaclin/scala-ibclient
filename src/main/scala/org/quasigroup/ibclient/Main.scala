package org.quasigroup.ibclient

import cats.effect.IOApp
import cats.effect.IO

object Main extends IOApp.Simple {

  def run: IO[Unit] =
    IBClient.twsClient[IO]().compile.drain
}
