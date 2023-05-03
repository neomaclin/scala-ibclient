package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.HistoricalTicksLast
import org.quasigroup.ibclient.types.{HistoricalTickLast, TickAttribLast, Decimal}

object HistoricalTicksLastReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[HistoricalTicksLast] =
    for
      reqId <- read[Int]
      tickCount <- read[Int]
      ticks <- (0 until tickCount).foldLeft(
        readNothing(List.empty[HistoricalTickLast])
      ) { (state, idx) =>
        for
          list <- state
          time <- read[Long]
          tickAttribLast <- read[TickAttribLast] // for consistency
          price <- read[Double]
          size <- read[Decimal]
          exchange <- read[String]
          specialConditions <- read[String]
        yield HistoricalTickLast(time, tickAttribLast, price, size, exchange, specialConditions) :: list
      }
      done <- read[Boolean]
    yield HistoricalTicksLast(reqId, ticks.reverse, done)
}
