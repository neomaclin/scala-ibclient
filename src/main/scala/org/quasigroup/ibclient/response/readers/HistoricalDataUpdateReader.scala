package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read}
import org.quasigroup.ibclient.response.ResponseMsg.HistoricalDataUpdate
import org.quasigroup.ibclient.types.{Bar, Decimal}

object HistoricalDataUpdateReader {
  val create: DecoderState[HistoricalDataUpdate] =
    for
      reqId <- read[Int]
      barCount <- read[Int]
      date <- read[String]
      open <- read[Double]
      close <- read[Double]
      high <- read[Double]
      low <- read[Double]
      wap <- read[Decimal]
      volume <- read[Decimal]
    yield HistoricalDataUpdate(
      reqId,
      Bar(
        time = date,
        open = open,
        high = high,
        low = low,
        close = close,
        volume = volume,
        count = barCount,
        wap = wap
      )
    )
}
