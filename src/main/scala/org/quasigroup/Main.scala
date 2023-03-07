package org.quasigroup

import cats.effect.IOApp
import cats.effect.IO

object Main extends IOApp.Simple {

  def run: IO[Unit] =
    IBClient.client[IO]().compile.drain
}
