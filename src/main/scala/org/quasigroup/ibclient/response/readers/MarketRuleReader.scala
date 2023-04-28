package org.quasigroup.ibclient.response.readers


import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.MarketRule
import org.quasigroup.ibclient.types.PriceIncrement

object MarketRuleReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[MarketRule] =
    for
      marketRuleId <- read[Int]
      nPriceIncrements <- read[Int]
      priceIncrements <-
        (0 until nPriceIncrements).foldLeft(
          readNothing(List.empty[PriceIncrement])
        ) { (state, idx) =>
          for
            list <- state
            lowEdge <- read[Double]
            increment <- read[Double]
          yield PriceIncrement(lowEdge, increment) :: list
        }
    yield MarketRule(marketRuleId, priceIncrements.reverse)
}
