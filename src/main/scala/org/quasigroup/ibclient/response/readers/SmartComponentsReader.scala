package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.SmartComponents

object SmartComponentsReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[SmartComponents] =
    for
      reqId <- read[Int]
      size <- read[Int]
      smartComponents <-
        (0 until size).foldLeft(
          readNothing(Map.empty[Int, (String, Char)])
        ) { (state, idx) =>
          for
            maps <- state
            bitNumber <- read[Int]
            exchange <- read[String]
            exchangeLetter <- read[String]
          yield maps + (bitNumber.toInt -> (exchange -> exchangeLetter.head))
        }
    yield SmartComponents(reqId, smartComponents)
}
