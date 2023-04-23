package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.SoftDollarTiers
import org.quasigroup.ibclient.types.SoftDollarTier

object SoftDollarTiersReader {
  val create: DecoderState[SoftDollarTiers] =
    for
      reqId <- read[Int]
      nTiers <- read[Int]
      tiers <-
        (0 until nTiers).foldLeft(readNothing(List.empty[SoftDollarTier])) { (state, idx) =>
          for
            list <- state
            name <- read[String]
            value <- read[String]
            displayName <- read[String]
          yield SoftDollarTier(name, value, displayName) :: list
        }
    yield SoftDollarTiers(reqId, tiers.reverse)
}
