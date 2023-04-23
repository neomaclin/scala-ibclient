package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.encoder.Encoder.{EncoderState, write}
import org.quasigroup.ibclient.request.RequestMsg.ReqRealTimeBars


object ReqRealTimeBarsWriter {
  def apply(a: ReqRealTimeBars): EncoderState =
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
