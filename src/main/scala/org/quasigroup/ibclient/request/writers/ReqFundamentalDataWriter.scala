package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.encoder.Encoder.{EncoderState,write,given}
import org.quasigroup.ibclient.request.RequestMsg.ReqFundamentalData


object ReqFundamentalDataWriter {
  def apply(a: ReqFundamentalData): EncoderState =
    for
      _ <- write(a.msgId)
      _ <- write(a.version)
      _ <- write(a.reqId)
      _ <- write(a.contract.conId)
      _ <- write(a.contract.symbol)
      _ <- write(a.contract.secType)
      _ <- write(a.contract.exchange)
      _ <- write(a.contract.primaryExch)
      _ <- write(a.contract.currency)
      _ <- write(a.contract.localSymbol)
      _ <- write(a.reportType)
      _ <- write(a.fundamentalDataOptions)
    yield ()

}
