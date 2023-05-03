package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.IBClient.*
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.OrderStatusMsg
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given

object OrderStatusMsgReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[OrderStatusMsg] =
    for
      version <-
        if serverVersion >= IBClient.MIN_SERVER_VER_MARKET_CAP_PRICE
        then readNothing(Int.MaxValue)
        else read[Int]
      id <- read[Int]
      status <- read[Order.Status]
      filled <- read[Decimal]
      remaining <- read[Decimal]
      avgFillPrice <- read[Double]
      permId <- if version >= 2 then read[Int] else readNothing(0)
      parentId <- if version >= 3 then read[Int] else readNothing(0)
      lastFillPrice <- if version >= 4 then read[Double] else readNothing(0.0)
      clientId <- if version >= 5 then read[Int] else readNothing(0)
      whyHeld <- if version >= 6 then read[String] else readNothing("")
      mktCapPrice <-
        if serverVersion >= IBClient.MIN_SERVER_VER_MARKET_CAP_PRICE then read[Double]
        else readNothing(Double.MaxValue)
    yield OrderStatusMsg(
      id,
      status,
      filled,
      remaining,
      avgFillPrice,
      permId,
      parentId,
      lastFillPrice,
      clientId,
      whyHeld,
      mktCapPrice
    )
}
