package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.IBClient.*
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.PnLSingle
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given

object PnLSingleMsgReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[PnLSingle] =
    for
      reqId <- read[Int]
      pos <- read[Decimal]
      dailyPnL <- read[Double]
      unrealizedPnL <-
        if serverVersion >= MIN_SERVER_VER_UNREALIZED_PNL then read[Double]
        else readNothing(Double.MaxValue)
      realizedPnL <-
        if serverVersion >= MIN_SERVER_VER_REALIZED_PNL then read[Double]
        else readNothing(Double.MaxValue)
      value <- read[Double]
    yield PnLSingle(reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value)
}
