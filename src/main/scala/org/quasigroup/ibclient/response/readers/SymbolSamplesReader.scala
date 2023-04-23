package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.types.{Contract,ContractDescription,SecType}
import org.quasigroup.ibclient.types.TypesCodec.given
import org.quasigroup.ibclient.response.ResponseMsg.SymbolSamples

object SymbolSamplesReader {
  val create: DecoderState[SymbolSamples] =
    for
      reqId <- read[Int]
      nContractDescriptions <- read[Int]
      contractDescriptions <- (0 until nContractDescriptions).foldLeft(
        readNothing(List.empty[ContractDescription])
      ) { (state, idx) =>
        for
          list <- state
          conid <- read[Int]
          symbol <- read[String]
          secType <- read[SecType]
          primaryExch <- read[String]
          currency <- read[String]
          nDerivativeSecTypes <- read[Int]
          derivativeSecTypes <-
            (0 until nDerivativeSecTypes).foldLeft(
              readNothing(List.empty[String])
            ) { (state, idx) =>
              for {
                derivativeSecTypes <- state
                derivativeSecType <- read[String]
              } yield derivativeSecType :: derivativeSecTypes
            }
          description <- read[String]
          issuerId <- read[String]
        yield {
          val contract =
            Contract(
              conId = conid,
              symbol = symbol,
              secType = secType,
              primaryExch = primaryExch,
              currency = currency,
              description = description,
              issuerId = issuerId
            )

          ContractDescription(contract, derivativeSecTypes.reverse) :: list
        }
      }
    yield SymbolSamples(reqId, contractDescriptions.reverse)

}
