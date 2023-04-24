package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.encoder.Encoder.{EncoderState, write}
import org.quasigroup.ibclient.request.RequestMsg.ReqContractDetails

import scala.collection.mutable

object ReqContractDetailsWriter {
  def apply(a: ReqContractDetails): EncoderState =
    for {
      _ <- write(a.msgId)
      _ <- write(a.version)
      _ <- ContractWriter(a.contract)
      _ <- write(a.contract.includeExpired)
      _ <- write(a.contract.secIdType)
      _ <- write(a.contract.secId)
      _ <- write(a.contract.issuerId)
    } yield ()
}
