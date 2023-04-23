package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.encoder.Encoder.{EncoderState, write,given}
import org.quasigroup.ibclient.types.TypesCodec.given
import org.quasigroup.ibclient.request.RequestMsg.ReqTickByTickData

object ReqTickByTickDataWriter{
  def apply(a: ReqTickByTickData): EncoderState =
    for
      _ <- write(a.msgId)
      _ <- write(a.reqId)
      _ <- ContractWriter(a.contract)
      _ <- write(a.tickType)
      _ <- write(a.numberOfTicks)
      _ <- write(a.ignoreSize)
    yield ()
}
