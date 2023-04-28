package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.encoder.Encoder.{EncoderState, write, given}
import org.quasigroup.ibclient.request.RequestMsg.CalculateOptionPrice

object CalculateOptionPriceWriter {
  def apply(a: CalculateOptionPrice)(using serverVersion: IBClient.ServerVersion): EncoderState =
    for
      _ <- write(a.msgId)
      _ <- write(a.version)
      _ <- write(a.reqId)
      _ <- ContractWriter(a.contract)
      _ <- write(a.volatility)
      _ <- write(a.underPrice)
      _ <- write(a.optionPriceOptions)
    yield ()
}
