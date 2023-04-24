package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.FamilyCodes
import org.quasigroup.ibclient.types.FamilyCode

object FamilyCodesReader {
  val create: DecoderState[FamilyCodes] =
    for
      numberOfFamilyCode <- read[Int]
      familyCodes <-
        (0 until numberOfFamilyCode).foldLeft(
          readNothing(List.empty[FamilyCode])
        ) { (state, idx) =>
          for
            list <- state
            accountID <- read[String]
            familyCodeStr <- read[String]
          yield FamilyCode(accountID, familyCodeStr) :: list
        }
    yield FamilyCodes(familyCodes.reverse)
}
