package org.quasigroup.ibclient.types

import java.time.Instant
import java.time.format.DateTimeFormatter
import java.time.ZoneId

object Util:
  def UnixMillisecondsToString(milliseconds: Long, dateFormat: String)(using zoneId: ZoneId): String =
    DateTimeFormatter
      .ofPattern(dateFormat)
      .withZone(zoneId)
      .format(Instant.ofEpochMilli(milliseconds))

  def UnixSecondsToString(seconds: Long, dateFormat: String)(using zoneId: ZoneId): String =
    UnixMillisecondsToString(seconds * 1000, dateFormat)
