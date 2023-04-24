package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.HistogramData
import org.quasigroup.ibclient.types.{HistogramEntry, Decimal}

object HistogramDataReader {
  val create: DecoderState[HistogramData] =
    for
      reqId <- read[Int]
      nEntries <- read[Int]
      entries <-
        (0 until nEntries).foldLeft(
          readNothing(List.empty[HistogramEntry])
        ) { (state, idx) =>
          for
            list <- state
            price <- read[Double]
            size <- read[Decimal]
          yield HistogramEntry(price, size) :: list
        }
    yield HistogramData(reqId, entries.reverse)
}
