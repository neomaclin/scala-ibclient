package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.PositionMulti
import org.quasigroup.ibclient.types.{Contract, ContractRight, SecType, Decimal}
import org.quasigroup.ibclient.types.TypesCodec.given

object PositionMultiReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[PositionMulti] =
    for
      reqId <- read[Int]
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
      tradingClass <- read[String]
      pos <- read[Decimal]
      avgCost <- read[Double]
      modelCode <- read[String]
    yield PositionMulti(
      reqId,
      account,
      modelCode,
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
