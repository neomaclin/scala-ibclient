package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg
import org.quasigroup.ibclient.response.ResponseMsg.{TickByTickAllLast,TickByTickBidAsk,TickByTickMidPoint,Skip}
import org.quasigroup.ibclient.types.{TickAttribBidAsk,TickAttribLast, Decimal}
import org.quasigroup.ibclient.types.TypesCodec.given

object TickByTicksReader{
  val create: DecoderState[ResponseMsg] =
    for
      reqId <- read[Int]
      tickType <- read[Int]
      time <- read[Long]
      tickByTickMsg <- tickType match
        case 1 | 2 =>
          for
            price <- read[Double]
            size <- read[Decimal]
            tickAttribLast <- read[TickAttribLast] // for consistency
            exchange <- read[String]
            specialConditions <- read[String]
          yield TickByTickAllLast(
            reqId,
            tickType,
            time,
            price,
            size,
            tickAttribLast,
            exchange,
            specialConditions
          )
        case 3 =>
          for
            bidPrice <- read[Double]
            askPrice <- read[Double]
            bidSize <- read[Decimal]
            askSize <- read[Decimal]
            tickAttribBidAsk <- read[TickAttribBidAsk] // for consistency
          yield TickByTickBidAsk(
            reqId,
            time,
            bidPrice,
            askPrice,
            bidSize,
            askSize,
            tickAttribBidAsk
          )
        case 4 =>
          for midPoint <- read[Double]
            yield TickByTickMidPoint(reqId, time, midPoint)
        case _ => readNothing(Skip)
    yield tickByTickMsg
}
