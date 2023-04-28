package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.IBClient

import org.quasigroup.ibclient.encoder.Encoder.{EncoderState, write}
import org.quasigroup.ibclient.request.RequestMsg.ReqRealTimeBars

object ReqRealTimeBarsWriter {
  def apply(a: ReqRealTimeBars)(using serverVersion: IBClient.ServerVersion): EncoderState =
    for {
      _ <- write(a.msgId)
      _ <- write(a.version)
      _ <- write(a.tickerId)
      _ <- ContractWriter(a.contract)
      _ <- write(a.barSize)
      _ <- write(a.whatToShow)
      _ <- write(a.useRTH)
      _ <- write(a.realTimeBarsOptions)
    } yield ()
}
