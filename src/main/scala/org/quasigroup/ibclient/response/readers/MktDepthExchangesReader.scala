package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.decoder.Decoder.DecoderState
import org.quasigroup.ibclient.types.{DepthMktDataDescription, SecType}
import org.quasigroup.ibclient.types.TypesCodec.given
import org.quasigroup.ibclient.response.ResponseMsg.MktDepthExchanges

object MktDepthExchangesReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[MktDepthExchanges] =
    for
      nDepthMktDataDescriptions <- read[Int]
      depthMktDataDescriptions <-
        (0 until nDepthMktDataDescriptions).foldLeft(
          readNothing(List.empty[DepthMktDataDescription])
        ) { (state, idx) =>
          for
            list <- state
            exchange <- read[String]
            secType <- read[SecType]
            listingExch <- read[String]
            serviceDataType <- read[String]
            aggGroup <- read[Int].map(value => if value == 0 then Int.MaxValue else value)
          yield DepthMktDataDescription(
            exchange,
            secType,
            listingExch,
            serviceDataType,
            aggGroup
          ) :: list
        }
    yield MktDepthExchanges(depthMktDataDescriptions.reverse)
}
