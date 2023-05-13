package org.quasigroup.ibclient.request

import RequestMsg.*
import writers.*
import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.{*, given}
import org.quasigroup.ibclient.encoder.Encoder
import org.quasigroup.ibclient.encoder.Encoder.{*, given}
import cats.MonadThrow
import cats.syntax.try_.*
import org.quasigroup.ibclient.request.writers.{ExerciseOptionsWriter, ReqMktDepthWriter}

import scala.collection.mutable
import scala.util.Try
object MsgEncoders:

  trait MsgEncoder[A] {
    def apply(a: A)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte]
  }

  inline def prependingLength(encoded: mutable.Buffer[Byte]): Array[Byte] =
    val length = encoded.length
    (Array(
      (0xff & (length >> 24)).toByte,
      (0xff & (length >> 16)).toByte,
      (0xff & (length >> 8)).toByte,
      (0xff & length).toByte
    ).toBuffer ++ encoded).toArray

  inline def encode[F[_]: MonadThrow, T: MsgEncoder](raw: T)(using
      serverVersion: IBClient.ServerVersion
  ): F[Array[Byte]] = {
    Try(prependingLength(summon[MsgEncoder[T]].apply(raw)(using serverVersion))).liftTo[F]
  }
  // Try(unsafeEncode(raw)).liftTo[F]

  inline def unsafeEncode[T: Encoder](raw: T): Array[Byte] =
    prependingLength(summon[Encoder[T]](raw))

  inline given MsgEncoder[ReqHistoricalData] with
    def apply(a: ReqHistoricalData)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      ReqHistoricalDataWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[ReqRealTimeBars] with
    def apply(a: ReqRealTimeBars)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      ReqRealTimeBarsWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[ReqContractDetails] with
    def apply(a: ReqContractDetails)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      ReqContractDetailsWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[ReqMktDepth] with
    def apply(a: ReqMktDepth)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      ReqMktDepthWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[ReqHistogramData] with
    def apply(a: ReqHistogramData)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      ReqHistogramDataWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[ReqMktData] with
    def apply(a: ReqMktData)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      ReqMktDataWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[ReqTickByTickData] with
    def apply(a: ReqTickByTickData)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      ReqTickByTickDataWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[ReqFundamentalData] with
    def apply(a: ReqFundamentalData)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      ReqFundamentalDataWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[CalculateImpliedVolatility] with
    def apply(a: CalculateImpliedVolatility)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      CalculateImpliedVolatilityWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[CalculateOptionPrice] with
    def apply(a: CalculateOptionPrice)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      CalculateOptionPriceWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[ExerciseOptions] with
    def apply(a: ExerciseOptions)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      ExerciseOptionsWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given MsgEncoder[PlaceOrder] with
    def apply(a: PlaceOrder)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] =
      PlaceOrderWriter(a)(using serverVersion).runS(mutable.Buffer.empty).value

  inline given genericMsgEncoder[T: Encoder]: MsgEncoder[T] = new MsgEncoder[T] {
    override def apply(a: T)(using serverVersion: IBClient.ServerVersion): mutable.Buffer[Byte] = summon[Encoder[T]](a)
  }

end MsgEncoders
