package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.IBClient

import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.encoder.Encoder.{*, given}
import org.quasigroup.ibclient.request.RequestMsg.ReqHistogramData

import scala.collection.mutable

object ReqHistogramDataWriter {
  def apply(a: ReqHistogramData)(using serverVersion: IBClient.ServerVersion): EncoderState =
    for {
      _ <- write(a.msgId)
      _ <- write(a.tickerId)
      _ <- ContractWriter(a.contract)
      _ <- write(a.contract.includeExpired)
      _ <- write(a.useRTH)
      _ <- write(a.timePeriod)
    } yield ()
}
