package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.encoder.Encoder.{EncoderState, write}
import org.quasigroup.ibclient.request.RequestMsg.ReqMktData

import scala.collection.mutable

object ReqMktDataWriter {
  def apply(a: ReqMktData): EncoderState =
    for {
      _ <- write(a.msgId)
      _ <- write(a.version)
      _ <- write(a.tickerId)
      _ <- ContractWriter(a.contract)
      _ <- a.contract.deltaNeutralContract match
        case Some(deltaNeutralContract) =>
          write(true).flatMap(_ => write(deltaNeutralContract))
        case None =>
          write(false)
      _ <- write(a.genericTickList)
      _ <- write(a.snapshot)
      _ <- write(a.regulatorySnapshot)
      _ <- write(a.mktDataOptions)
    } yield ()
}
