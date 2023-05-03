package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.{HistoricalData, HistoricalDataUpdate}
import org.quasigroup.ibclient.types.{Bar, Decimal}

object HistoricalDataReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[HistoricalData] =
    for
      version <- read[Int]
      reqId <- read[Int]
      startDate <- if (version >= 2) then read[String] else readNothing("")
      endDate <- if (version >= 2) then read[String] else readNothing("")
      itemCount <- read[Int]
      histories <-
        (0 until itemCount).foldLeft(
          readNothing(List.empty[HistoricalDataUpdate])
        ) { (state, idx) =>
          for
            list <- state
            date <- read[String]
            open <- read[Double]
            high <- read[Double]
            low <- read[Double]
            close <- read[Double]
            volume <- read[Decimal]
            wap <- read[Decimal]
            barCount <- if version >= 3 then read[Int] else readNothing(-1)
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
          ) :: list
        }
    yield HistoricalData(histories.reverse, startDate, endDate)
}
