package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.UpdateAccountValue

object UpdateAccountValueReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[UpdateAccountValue] =
    for
      version <- read[Int]
      key <- read[String]
      value <- read[String]
      cur <- read[String]
      accountName <- if version >= 2 then read[String] else readNothing("")
    yield UpdateAccountValue(key, value, cur, accountName)
}
