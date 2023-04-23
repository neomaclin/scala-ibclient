package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.HistoricalTicksBidAsk
import org.quasigroup.ibclient.types.{HistoricalTickBidAsk, TickAttribBidAsk,Decimal}

object HistoricalTicksBidAskReader {
  val create: DecoderState[HistoricalTicksBidAsk] =
    for
      reqId <- read[Int]
      tickCount <- read[Int]
      ticks <- (0 until tickCount).foldLeft(
        readNothing(List.empty[HistoricalTickBidAsk])
      ) { (state, idx) =>
        for
          list <- state
          time <- read[Long]
          tickAttribBidAsk <- read[TickAttribBidAsk] // for consistency
          priceBid <- read[Double]
          priceAsk <- read[Double]
          sizeBid <- read[Decimal]
          sizeAsk <- read[Decimal]
        yield HistoricalTickBidAsk(time, tickAttribBidAsk, priceBid, priceAsk, sizeBid, sizeAsk) :: list
      }
      done <- read[Boolean]
    yield HistoricalTicksBidAsk(reqId, ticks.reverse, done)
}
