package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.encoder.Encoder.{EncoderState,write,given}
import org.quasigroup.ibclient.request.RequestMsg.ExerciseOptions

object ExerciseOptionsWriter {
  def apply(a: ExerciseOptions): EncoderState =
    for
      _ <- write(a.msgId)
      _ <- write(a.version)
      _ <- write(a.tickerId)
      _ <- ContractWriter(a.contract)
      _ <- write(a.exerciseAction)
      _ <- write(a.exerciseQuantity)
      _ <- write(a.account)
      _ <- write(a.`override`)
    yield ()
}
