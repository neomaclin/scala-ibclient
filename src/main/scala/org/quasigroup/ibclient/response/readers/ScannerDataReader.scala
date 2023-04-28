package org.quasigroup.ibclient.response.readers


import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.ScannerData
import org.quasigroup.ibclient.types.{Contract, ContractRight, SecType, ScannerDataElement}
import org.quasigroup.ibclient.types.TypesCodec.given
import org.quasigroup.ibclient.types.ContractDetails

object ScannerDataReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[ScannerData] =
    for
      version <- read[Int]
      tickerId <- read[Int]
      numberOfElements <- read[Int]
      elements <-
        (0 until numberOfElements).foldLeft(
          readNothing(List.empty[ScannerDataElement])
        ) { (state, idx) =>
          for
            list <- state
            rank <- read[Int]
            conid <- if (version >= 3) then read[Int] else readNothing(-1)
            symbol <- read[String]
            secType <- read[SecType]
            lastTradeDateOrContractMonth <- read[String]
            strike <- read[Double]
            right <- read[ContractRight]
            multiplier <- read[String]
            exchange <- read[String]
            currency <- read[String]
            localSymbol <- read[String]
            marketName <- read[String]
            tradingClass <- read[String]
            distance <- read[String]
            benchmark <- read[String]
            projection <- read[String]
            legsStr <- if (version >= 2) then read[String] else readNothing("")
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
            val contractDetails = ContractDetails(
              contract = contract,
              marketName = marketName
            )
            ScannerDataElement(
              rank = rank,
              contractDetails = contractDetails,
              distance = distance,
              benchmark = benchmark,
              projection = projection,
              legsStr = legsStr
            ) :: list
          }
        }
    yield ScannerData(tickerId, elements.reverse)
}
