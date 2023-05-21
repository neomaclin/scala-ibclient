package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.encoder.Encoder.{EncoderState, write, given}

import org.quasigroup.ibclient.types.TypesCodec.given
import org.quasigroup.ibclient.types.Contract

object ContractWriter {
  def apply(contract: Contract)(using serverVersion: IBClient.ServerVersion): EncoderState =
    for
      _ <- write(contract.conId)
      _ <- write(contract.symbol)
      _ <- write(contract.secType)
      _ <- write(contract.lastTradeDateOrContractMonth)
      _ <- write(contract.strike)
      _ <- write(contract.right)
      _ <- write(contract.multiplier)
      _ <- write(contract.exchange)
      _ <- write(contract.primaryExch)
      _ <- write(contract.currency)
      _ <- write(contract.localSymbol)
      _ <- write(contract.tradingClass)
    yield ()
}
