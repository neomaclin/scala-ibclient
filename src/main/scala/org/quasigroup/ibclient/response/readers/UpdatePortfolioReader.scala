package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.UpdatePortfolio
import org.quasigroup.ibclient.types.{Contract, ContractRight, SecType, Decimal}
import org.quasigroup.ibclient.types.TypesCodec.given

object UpdatePortfolioReader {
  val create: DecoderState[UpdatePortfolio] =
    for
      version <- read[Int]
      conId <- if version >= 6 then read[Int] else readNothing(0)
      symbol <- read[String]
      secType <- read[SecType]
      lastTradeDateOrContractMonth <- read[String]
      strike <- read[Double]
      right <- read[ContractRight]
      multiplier <-
        if version >= 7 then read[String] else readNothing("")
      primaryExch <-
        if version >= 7 then read[String] else readNothing("")
      currency <- read[String]
      localSymbol <-
        if version >= 2 then read[String] else readNothing("")
      tradingClass <-
        if version >= 8 then read[String] else readNothing("")
      position <- read[Decimal]
      marketPrice <- read[Double]
      marketValue <- read[Double]
      averageCost <-
        if version >= 3 then read[Double] else readNothing(0.0)
      unrealizedPNL <-
        if version >= 3 then read[Double] else readNothing(0.0)
      realizedPNL <-
        if version >= 3 then read[Double] else readNothing(0.0)
      accountName <-
        if version >= 4 then read[String] else readNothing("")
    yield
      val contract = Contract(
        conId = conId,
        symbol = symbol,
        secType = secType,
        lastTradeDateOrContractMonth = lastTradeDateOrContractMonth,
        strike = strike,
        right = right,
        multiplier = multiplier,
        primaryExch = primaryExch,
        currency = currency,
        localSymbol = localSymbol,
        tradingClass = tradingClass
      )
      UpdatePortfolio(
        contract,
        position,
        marketPrice,
        marketValue,
        averageCost,
        unrealizedPNL,
        realizedPNL,
        accountName
      )
}
