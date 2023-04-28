package org.quasigroup.ibclient.response.readers


import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.ExecDetails

object ExecutionDataReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[ExecDetails] =
    for
      version <- read[Int]
      reqId <- if version >= 7 then read[Int] else readNothing(-1)
      orderId <- read[Int]
      conid <- if version >= 5 then read[Int] else readNothing(-1)
      symbol <- read[String]
      secType <- read[SecType]
      lastTradeDateOrContractMonth <- read[String]
      strike <- read[Double]
      right <- read[ContractRight]
      multiplier <- if version >= 9 then read[String] else readNothing("")
      exchange <- read[String]
      currency <- read[String]
      localSymbol <- read[String]
      marketName <- read[String]
      tradingClass <- if version >= 10 then read[String] else readNothing("")
      execId <- read[String]
      time <- read[String]
      acctNumber <- read[String]
      exeExchange <- read[String]
      side <- read[String]
      shares <- read[Decimal]
      exePrice <- read[Double]
      permId <- if version >= 2 then read[Int] else readNothing(-1)
      clientId <- if version >= 3 then read[Int] else readNothing(-1)
      liquidation <- if version >= 4 then read[Int] else readNothing(0)
      cumQty <- if version >= 6 then read[Decimal] else readNothing(Decimal.INVALID)
      avgPrice <- if version >= 6 then read[Double] else readNothing(Double.MaxValue)
      orderRef <- if version >= 8 then read[String] else readNothing("")
      evRule <- if version >= 9 then read[String] else readNothing("")
      evMultiplier <- if version >= 9 then read[Double] else readNothing(Double.MaxValue)
      modelCode <- read[String]
      lastLiquidity <- read[Liquidities]
    yield {
      val contract = Contract(
        conId = conid,
        symbol = symbol,
        secType = secType,
        lastTradeDateOrContractMonth = lastTradeDateOrContractMonth,
        strike = strike,
        right = right,
        multiplier = multiplier,
        exchange = exchange,
        currency = currency,
        localSymbol = localSymbol,
        tradingClass = tradingClass
      )
      val execution = Execution(
        orderId = orderId,
        execId = execId,
        time = time,
        acctNumber = acctNumber,
        exchange = exeExchange,
        side = side,
        shares = shares,
        price = exePrice,
        permId = permId,
        clientId = clientId,
        liquidation = liquidation,
        cumQty = cumQty,
        avgPrice = avgPrice,
        orderRef = orderRef,
        evRule = evRule,
        evMultiplier = evMultiplier,
        modelCode = modelCode,
        lastLiquidity = lastLiquidity
      )

      ExecDetails(reqId, contract, execution)
    }
}
