package org.quasigroup.ibclient.response.readers



import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.HistoricalTicks
import org.quasigroup.ibclient.types.{HistoricalTick, Decimal}

object HistoricalTicksReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[HistoricalTicks] =
    for
      reqId <- read[Int]
      tickCount <- read[Int]
      ticks <- (0 until tickCount).foldLeft(
        readNothing(List.empty[HistoricalTick])
      ) { (state, idx) =>
        for
          list <- state
          time <- read[Long]
          _ <- read[Int] // for consistency
          price <- read[Double]
          size <- read[Decimal]
        yield HistoricalTick(time, price, size) :: list
      }
      done <- read[Boolean]
    yield HistoricalTicks(reqId, ticks.reverse, done)
}
