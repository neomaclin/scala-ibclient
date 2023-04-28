package org.quasigroup.ibclient.response.readers


import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.HistoricalSchedule
import org.quasigroup.ibclient.types.{HistoricalSession, Decimal}
object HistoricalScheduleReader {

  def create(using serverVersion: IBClient.ServerVersion): DecoderState[HistoricalSchedule] =
    for
      reqId <- read[Int]
      startDateTime <- read[String]
      endDateTime <- read[String]
      timeZone <- read[String]
      sessionsCount <- read[Int]
      sessions <-
        (0 until sessionsCount).foldLeft(
          readNothing(List.empty[HistoricalSession])
        ) { (state, idx) =>
          for
            list <- state
            sessionStartDateTime <- read[String]
            sessionEndDateTime <- read[String]
            sessionRefDateTime <- read[String]
          yield HistoricalSession(
            sessionStartDateTime,
            sessionEndDateTime,
            sessionRefDateTime
          ) :: list
        }
    yield HistoricalSchedule(
      reqId,
      startDateTime,
      endDateTime,
      timeZone,
      sessions.reverse
    )
}
