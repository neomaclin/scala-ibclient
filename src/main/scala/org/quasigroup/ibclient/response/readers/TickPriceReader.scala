package org.quasigroup.ibclient.response.readers


import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.{TickPrice, TickSize}
import org.quasigroup.ibclient.types.{Decimal, TickAttrib, TickType}
import org.quasigroup.ibclient.types.TypesCodec.given

import cats.syntax.option.*
object TickPriceReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[TickPrice] =
    for
      version <- read[Int]
      tickerId <- read[Int]
      tickType <- read[TickType]
      price <- read[Double]
      size <-
        if version >= 2 then read[Decimal] else readNothing(Decimal.INVALID)
      tickAttrib <-
        if version >= 3 then read[TickAttrib] else readNothing(TickAttrib())
    yield {
      val optionalSizeTickType =
        if version >= 2 then
          tickType match
            case TickType.BID          => TickType.BID_SIZE.some
            case TickType.ASK          => TickType.ASK_SIZE.some
            case TickType.LAST         => TickType.LAST_SIZE.some
            case TickType.DELAYED_BID  => TickType.DELAYED_BID_SIZE.some
            case TickType.DELAYED_ASK  => TickType.DELAYED_ASK_SIZE.some
            case TickType.DELAYED_LAST => TickType.DELAYED_LAST_SIZE.some
            case _                     => None
        else None
      TickPrice(
        tickerId,
        tickType,
        price,
        tickAttrib,
        optionalSizeTickType.map(TickSize(tickerId, _, size))
      )
    }
}
