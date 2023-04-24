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

  inline def prependingLength(encoded: mutable.Buffer[Byte]): Array[Byte] =
    val length = encoded.length
    (Array(
      (0xff & (length >> 24)).toByte,
      (0xff & (length >> 16)).toByte,
      (0xff & (length >> 8)).toByte,
      (0xff & length).toByte
    ).toBuffer ++ encoded).toArray

  inline def encode[F[_]: MonadThrow, T: Encoder](raw: T)(using
      serverVersion: IBClient.ServerVersion
  ): F[Array[Byte]] =
    Try(unsafeEncode(raw)).liftTo[F]

  inline def unsafeEncode[T: Encoder](raw: T): Array[Byte] =
    prependingLength(summon[Encoder[T]](raw))

  inline given Encoder[ReqHistoricalData] = ReqHistoricalDataWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[ReqRealTimeBars] = ReqRealTimeBarsWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[ReqContractDetails] = ReqContractDetailsWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[ReqMktDepth] = ReqMktDepthWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[ReqMktData] = ReqMktDataWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[ReqTickByTickData] = ReqTickByTickDataWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[ReqFundamentalData] = ReqFundamentalDataWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[CalculateImpliedVolatility] =
    CalculateImpliedVolatilityWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[CalculateOptionPrice] = CalculateOptionPriceWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[ExerciseOptions] = ExerciseOptionsWriter(_).runS(mutable.Buffer.empty).value

  inline given Encoder[PlaceOrder] = PlaceOrderWriter(_).runS(mutable.Buffer.empty).value

end MsgEncoders
