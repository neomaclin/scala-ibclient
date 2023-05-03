package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.NewsProviders
import org.quasigroup.ibclient.types.{NewsProvider, Decimal}

object NewsProvidersReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[NewsProviders] =
    for
      nNewsProviders <- read[Int]
      newsProviders <-
        (0 until nNewsProviders).foldLeft(
          readNothing(List.empty[NewsProvider])
        ) { (state, idx) =>
          for
            list <- state
            providerCode <- read[String]
            providerName <- read[String]
          yield NewsProvider(providerCode, providerName) :: list
        }
    yield NewsProviders(newsProviders.reverse)
}
