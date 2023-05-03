package org.quasigroup.ibclient.types

object Formats:
  private val FMT2 = "#,##0.00"
  private val FMT0 = "#,##0"
  private val PCT = "0.0%"
  private val DATE_TIME = "yyyy-MM-dd HH:mm:ss" // format for display
  private val TIME = "HH:mm:ss" // format for display
  def decodeUnicodeEscapedString(str: String): String = ???
