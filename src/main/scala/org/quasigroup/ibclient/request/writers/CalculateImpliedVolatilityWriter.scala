package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.encoder.Encoder.{EncoderState, write}
import org.quasigroup.ibclient.request.RequestMsg.CalculateImpliedVolatility

import scala.collection.mutable
import org.quasigroup.ibclient.IBClient

object CalculateImpliedVolatilityWriter {

  def apply(a: CalculateImpliedVolatility)(using serverVersion: IBClient.ServerVersion): EncoderState =
    for
      _ <- write(a.msgId)
      _ <- write(a.version)
      _ <- write(a.reqId)
      _ <- ContractWriter(a.contract)
      _ <- write(a.optionPrice)
      _ <- write(a.underPrice)
      _ <- write(a.impliedVolatilityOptions)
    yield ()
}
