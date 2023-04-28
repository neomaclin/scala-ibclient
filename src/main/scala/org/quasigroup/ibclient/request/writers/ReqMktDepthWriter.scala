package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.IBClient

import org.quasigroup.ibclient.encoder.Encoder.{EncoderState, write}
import org.quasigroup.ibclient.request.RequestMsg.ReqMktDepth

import scala.collection.mutable

object ReqMktDepthWriter {
  def apply(a: ReqMktDepth)(using serverVersion: IBClient.ServerVersion): EncoderState =
    for {
      _ <- write(a.msgId)
      _ <- write(a.version)
      _ <- write(a.tickerId)
      _ <- ContractWriter(a.contract)
      _ <- write(a.numRows)
      _ <- write(a.isSmartDepth)
      _ <- write(a.mktDepthOptions)
    } yield ()
}
