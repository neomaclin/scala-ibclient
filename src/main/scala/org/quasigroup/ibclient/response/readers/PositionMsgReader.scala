package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.PositionMsg
import org.quasigroup.ibclient.types.{Contract, ContractRight, SecType, Decimal}
import org.quasigroup.ibclient.types.TypesCodec.given

object PositionMsgReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[PositionMsg] =
    for
      version <- read[Int]
      account <- read[String]
      conId <- read[Int]
      symbol <- read[String]
      secType <- read[SecType]
      lastTradeDateOrContractMonth <- read[String]
      strike <- read[Double]
      right <- read[ContractRight]
      multiplier <- read[String]
      exchange <- read[String]
      currency <- read[String]
      localSymbol <- read[String]
      tradingClass <-
        if version >= 2 then read[String] else readNothing("")
      pos <- read[Decimal]
      avgCost <- if version >= 3 then read[Double] else readNothing(Double.MaxValue)
    yield PositionMsg(
      account,
      Contract(
        conId,
        symbol,
        secType,
        lastTradeDateOrContractMonth,
        strike,
        right,
        multiplier,
        exchange,
        currency,
        localSymbol,
        tradingClass
      ),
      pos,
      avgCost
    )
}
