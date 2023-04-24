package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.ContractDetailsMsg
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given

object ContractDataReader {
  val create: DecoderState[ContractDetailsMsg] =
    for
      version <- read[Int]
      reqId <- if version >= 3 then read[Int] else readNothing(-1)
      symbol <- read[String]
      secType <- read[SecType]
      lastTradeDateOrContractMonth <- read[String]
      strike <- read[Double]
      right <- read[ContractRight]
      exchange <- read[String]
      currency <- read[String]
      localSymbol <- read[String]
      marketName <- read[String]
      tradingClass <- read[String]
      conid <- read[Int]
      minTick <- read[Double]
      _ <- read[Int] // mdSizeMultiplier - not used anymore
      orderTypes <- read[String]
      validExchanges <- read[String]
      priceMagnifier <- if version >= 2 then read[Int] else readNothing(0)
      underConId <- if version >= 4 then read[Int] else readNothing(0)
      longName <- if version >= 5 then read[String] else readNothing("")
      primaryExch <- if version >= 5 then read[String] else readNothing("")
      contractMonth <- if version >= 6 then read[String] else readNothing("")
      industry <- if version >= 6 then read[String] else readNothing("")
      category <- if version >= 6 then read[String] else readNothing("")
      subcategory <- if version >= 6 then read[String] else readNothing("")
      timeZoneId <- if version >= 6 then read[String] else readNothing("")
      tradingHours <- if version >= 6 then read[String] else readNothing("")
      liquidHours <- if version >= 6 then read[String] else readNothing("")
      evRule <- if version >= 8 then read[String] else readNothing("")
      evMultiplier <- if version >= 8 then read[Double] else readNothing(Double.MaxValue)
      secIdListCount <- if version >= 7 then read[Int] else readNothing(0)
      secIdList <- (0 until secIdListCount).foldLeft(readNothing(List.empty[TagValue])) { (state, idx) =>
        for
          list <- state
          tag <- read[String]
          value <- read[String]
        yield TagValue(tag, value) :: list
      }
      aggGroup <- read[Int]
      underSymbol <- read[String]
      underSecType <- read[SecType]
      marketRuleIds <- read[String]
      realExpirationDate <- read[String]
      stockType <- read[String]
      _ <- read[Decimal] // sizeMinTick - not used anymore
      minSize <- read[Decimal]
      sizeIncrement <- read[Decimal]
      suggestedSizeIncrement <- read[Decimal]
    yield {
      val (contractMonth, lastTradeTime) =
        if lastTradeDateOrContractMonth.nonEmpty then
          val splitted =
            if lastTradeDateOrContractMonth.contains("-") then lastTradeDateOrContractMonth.split("-")
            else lastTradeDateOrContractMonth.split("\\s+")
          splitted match
            case Array(contractMonth, lastTradeTime, rest: _*) => (contractMonth, lastTradeTime)
            case Array(contractMonth, rest: _*)                => (contractMonth, "")
            case _                                             => ("", "")
        else ("", "")

      val contract = Contract(
        conId = conid,
        symbol = symbol,
        secType = secType,
        strike = strike,
        right = right,
        exchange = exchange,
        currency = currency,
        localSymbol = localSymbol,
        tradingClass = tradingClass,
        lastTradeDateOrContractMonth = contractMonth
      )
      val contractDetails = ContractDetails(
        contract = contract,
        priceMagnifier = priceMagnifier,
        underConId = underConId,
        underSymbol = underSymbol,
        underSecType = underSecType,
        minTick = minTick,
        // descAppend = descAppend,
        marketName = marketName,
        orderTypes = orderTypes,
        validExchanges = validExchanges,
        contractMonth = contractMonth,
        subcategory = subcategory,
        category = category,
        industry = industry,
        longName = longName,
        evMultiplier = evMultiplier,
        evRule = evRule,
        secIdList = secIdList,
        aggGroup = aggGroup,
        marketRuleIds = marketRuleIds,
        lastTradeTime = lastTradeTime,
        liquidHours = liquidHours,
        tradingHours = tradingHours,
        timeZoneId = timeZoneId,
        stockType = stockType,
        minSize = minSize,
        sizeIncrement = sizeIncrement,
        suggestedSizeIncrement = suggestedSizeIncrement
      )

      ContractDetailsMsg(reqId, contractDetails)
    }

}
