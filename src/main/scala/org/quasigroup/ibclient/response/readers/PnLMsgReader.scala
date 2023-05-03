package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.IBClient.*
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.PnL

object PnLMsgReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[PnL] =
    for
      reqId <- read[Int]
      dailyPnL <- read[Double]
      unrealizedPnL <-
        if serverVersion >= MIN_SERVER_VER_UNREALIZED_PNL then read[Double]
        else readNothing(Double.MaxValue)
      realizedPnL <-
        if serverVersion >= MIN_SERVER_VER_REALIZED_PNL then read[Double]
        else readNothing(Double.MaxValue)
    yield PnL(reqId, dailyPnL, unrealizedPnL, realizedPnL)
}
