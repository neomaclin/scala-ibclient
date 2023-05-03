package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.SecurityDefinitionOptionalParameter

object SecurityDefinitionOptionalParameterReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[SecurityDefinitionOptionalParameter] =
    for
      reqId <- read[Int]
      exchange <- read[String]
      underlyingConId <- read[Int]
      tradingClass <- read[String]
      multiplier <- read[String]
      expirationsSize <- read[Int]
      expirations <- (0 until expirationsSize).foldLeft(
        readNothing(Set.empty[String])
      ) { (state, idx) =>
        for
          set <- state
          expiration <- read[String]
        yield set + expiration
      }
      strikesSize <- read[Int]
      strikes <- (0 until strikesSize).foldLeft(
        readNothing(Set.empty[Double])
      ) { (state, idx) =>
        for
          set <- state
          strike <- read[Double]
        yield set + strike
      }
    yield SecurityDefinitionOptionalParameter(
      reqId,
      exchange,
      underlyingConId,
      tradingClass,
      multiplier,
      expirations,
      strikes
    )
}
