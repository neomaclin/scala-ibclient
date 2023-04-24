package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.BondContractDetails
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given

object BondContractDataReader {
  val create: DecoderState[BondContractDetails] =
    for
      version <- read[Int]
      reqId <- if version >= 3 then read[Int] else readNothing(-1)
      symbol <- read[String]
      secType <- read[SecType]
      cusip <- read[String]
      coupon <- read[Double]
      lastTradeDateOrContractMonth <- read[String]
      issueDate <- read[String]
      ratings <- read[String]
      bondType <- read[String]
      couponType <- read[String]
      convertible <- read[Boolean]
      callable <- read[Boolean]
      putable <- read[Boolean]
      descAppend <- read[String]
      exchange <- read[String]
      currency <- read[String]
      marketName <- read[String]
      tradingClass <- read[String]
      conid <- read[Int]
      minTick <- read[Double]
      _ <- read[Int]
      orderTypes <- read[String]
      validExchanges <- read[String]
      nextOptionDate <- if version >= 2 then read[String] else readNothing("")
      nextOptionType <- if version >= 2 then read[String] else readNothing("")
      nextOptionPartial <- if version >= 2 then read[Boolean] else readNothing(false)
      notes <- if version >= 2 then read[String] else readNothing("")
      longName <- if version >= 4 then read[String] else readNothing("")
      evRule <- if version >= 6 then read[String] else readNothing("")
      evMultiplier <- if version >= 6 then read[Double] else readNothing(Double.MaxValue)
      secIdListCount <- if version >= 5 then read[Int] else readNothing(0)
      secIdList <- (0 until secIdListCount).foldLeft(readNothing(List.empty[TagValue])) { (state, idx) =>
        for
          list <- state
          tag <- read[String]
          value <- read[String]
        yield TagValue(tag, value) :: list
      }
      aggGroup <- read[Int]
      marketRuleIds <- read[String]
      minSize <- read[Decimal]
      sizeIncrement <- read[Decimal]
      suggestedSizeIncrement <- read[Decimal]
    yield {
      val (maturity, lastTradeTime, timeZoneId) =
        if lastTradeDateOrContractMonth.nonEmpty then
          val splitted =
            if lastTradeDateOrContractMonth.contains("-") then lastTradeDateOrContractMonth.split("-")
            else lastTradeDateOrContractMonth.split("\\s+")
          splitted match
            case Array(maturity, lastTradeTime, timeZoneId, rest: _*) => (maturity, lastTradeTime, timeZoneId)
            case Array(maturity, lastTradeTime, rest: _*)             => (maturity, lastTradeTime, "")
            case Array(maturity, rest: _*)                            => (maturity, "", "")
            case _                                                    => ("", "", "")
        else ("", "", "")

      val contract = Contract(
        conId = conid,
        symbol = symbol,
        secType = secType,
        exchange = exchange,
        currency = currency,
        tradingClass = tradingClass
      )
      val contractDetails = ContractDetails(
        contract = contract,
        minTick = minTick,
        cusip = cusip,
        coupon = coupon,
        issueDate = issueDate,
        ratings = ratings,
        bondType = bondType,
        couponType = couponType,
        convertible = convertible,
        callable = callable,
        putable = putable,
        descAppend = descAppend,
        marketName = marketName,
        orderTypes = orderTypes,
        validExchanges = validExchanges,
        nextOptionDate = nextOptionDate,
        nextOptionType = nextOptionType,
        nextOptionPartial = nextOptionPartial,
        notes = notes,
        longName = longName,
        evMultiplier = evMultiplier,
        evRule = evRule,
        secIdList = secIdList,
        aggGroup = aggGroup,
        marketRuleIds = marketRuleIds,
        maturity = maturity,
        lastTradeTime = lastTradeTime,
        timeZoneId = timeZoneId,
        minSize = minSize,
        sizeIncrement = sizeIncrement,
        suggestedSizeIncrement = suggestedSizeIncrement
      )
      BondContractDetails(reqId, contractDetails)
    }
}
