package org.quasigroup.ibclient.request

import RequestMsg.*
import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given
import org.quasigroup.ibclient.encoder.Encoder
import org.quasigroup.ibclient.encoder.Encoder.given

import cats.MonadThrow
import cats.syntax.try_.*
import scala.collection.mutable
import scala.util.Try
object MsgEncoders {

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

  inline given Encoder[ReqHistoricalData] with
    override def apply(a: ReqHistoricalData): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.version)
        ++ summon[Encoder[Int]](a.tickerId)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](a.contract.strike)
        ++ summon[Encoder[ContractRight]](a.contract.right)
        ++ summon[Encoder[String]](a.contract.multiplier)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.contract.tradingClass)
        ++ summon[Encoder[String]](a.endDateTime)
        ++ summon[Encoder[String]](a.barSizeSetting)
        ++ summon[Encoder[String]](a.durationStr)
        ++ summon[Encoder[Int]](a.useRTH)
        ++ summon[Encoder[String]](a.whatToShow)
        ++ summon[Encoder[Int]](a.formatDate)
        ++ (
          if a.contract.secType == SecType.BAG then
            summon[Encoder[Int]](a.contract.comboLegs.size)
            a.contract.comboLegs.foldLeft(mutable.Buffer.empty) {
              (buffer, comboLeg) =>
                buffer ++
                  summon[Encoder[Int]](comboLeg.conId)
                summon[Encoder[Int]](comboLeg.ratio)
                summon[Encoder[Action]](comboLeg.action)
                summon[Encoder[String]](comboLeg.exchange)
            }
          else mutable.Buffer.empty
        )
        ++ summon[Encoder[Boolean]](a.keepUpToDate)
        ++ summon[Encoder[List[TagValue]]](a.chartOptions)
  end given

  inline given Encoder[ReqRealTimeBars] with
    override def apply(a: ReqRealTimeBars): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.version)
        ++ summon[Encoder[Int]](a.tickerId)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](a.contract.strike)
        ++ summon[Encoder[ContractRight]](a.contract.right)
        ++ summon[Encoder[String]](a.contract.multiplier)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.contract.tradingClass)
        ++ summon[Encoder[Int]](a.barSize)
        ++ summon[Encoder[String]](a.whatToShow)
        ++ summon[Encoder[Boolean]](a.useRTH)
        ++ summon[Encoder[List[TagValue]]](a.realTimeBarsOptions)
  end given

  inline given Encoder[ReqContractDetails] with
    override def apply(a: ReqContractDetails): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.version)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](a.contract.strike)
        ++ summon[Encoder[ContractRight]](a.contract.right)
        ++ summon[Encoder[String]](a.contract.multiplier)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.contract.tradingClass)
        ++ summon[Encoder[Boolean]](a.contract.includeExpired)
        ++ summon[Encoder[SecIdType]](a.contract.secIdType)
        ++ summon[Encoder[String]](a.contract.secId)
        ++ summon[Encoder[String]](a.contract.issuerId)
  end given

  inline given Encoder[ReqMktDepth] with
    override def apply(a: ReqMktDepth): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.version)
        ++ summon[Encoder[Int]](a.tickerId)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](a.contract.strike)
        ++ summon[Encoder[ContractRight]](a.contract.right)
        ++ summon[Encoder[String]](a.contract.multiplier)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.contract.tradingClass)
        ++ summon[Encoder[Int]](a.numRows)
        ++ summon[Encoder[Boolean]](a.isSmartDepth)
        ++ summon[Encoder[List[TagValue]]](a.mktDepthOptions)
  end given

  inline given Encoder[ReqMktData] with
    override def apply(a: ReqMktData): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.version)
        ++ summon[Encoder[Int]](a.tickerId)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](a.contract.strike)
        ++ summon[Encoder[ContractRight]](a.contract.right)
        ++ summon[Encoder[String]](a.contract.multiplier)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.contract.tradingClass)
        ++ (
          a.contract.deltaNeutralContract match
            case Some(deltaNeutralContract) =>
              summon[Encoder[Boolean]](true)
                ++ summon[Encoder[DeltaNeutralContract]](deltaNeutralContract)
            case None => summon[Encoder[Boolean]](false)
        )
        ++ summon[Encoder[String]](a.genericTickList)
        ++ summon[Encoder[Boolean]](a.snapshot)
        ++ summon[Encoder[Boolean]](a.regulatorySnapshot)
        ++ summon[Encoder[List[TagValue]]](a.mktDataOptions)
  end given

  inline given Encoder[ReqTickByTickData] with
    override def apply(a: ReqTickByTickData): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.reqId)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](a.contract.strike)
        ++ summon[Encoder[ContractRight]](a.contract.right)
        ++ summon[Encoder[String]](a.contract.multiplier)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.contract.tradingClass)
        ++ summon[Encoder[TickType]](a.tickType)
        ++ summon[Encoder[Int]](a.numberOfTicks)
        ++ summon[Encoder[Boolean]](a.ignoreSize)
  end given

  inline given Encoder[ReqFundamentalData] with
    override def apply(a: ReqFundamentalData): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.version)
        ++ summon[Encoder[Int]](a.reqId)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.reportType)
        ++ summon[Encoder[List[TagValue]]](a.fundamentalDataOptions)
  end given

  inline given Encoder[CalculateImpliedVolatility] with
    override def apply(a: CalculateImpliedVolatility): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.version)
        ++ summon[Encoder[Int]](a.reqId)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](a.contract.strike)
        ++ summon[Encoder[ContractRight]](a.contract.right)
        ++ summon[Encoder[String]](a.contract.multiplier)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.contract.tradingClass)
        ++ summon[Encoder[Double]](a.optionPrice)
        ++ summon[Encoder[Double]](a.underPrice)
        ++ summon[Encoder[List[TagValue]]](a.impliedVolatilityOptions)
  end given

  inline given Encoder[CalculateOptionPrice] with
    override def apply(a: CalculateOptionPrice): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.version)
        ++ summon[Encoder[Int]](a.reqId)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](a.contract.strike)
        ++ summon[Encoder[ContractRight]](a.contract.right)
        ++ summon[Encoder[String]](a.contract.multiplier)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.contract.tradingClass)
        ++ summon[Encoder[Double]](a.volatility)
        ++ summon[Encoder[Double]](a.underPrice)
        ++ summon[Encoder[List[TagValue]]](a.optionPriceOptions)
  end given

  inline given Encoder[ExerciseOptions] with
    override def apply(a: ExerciseOptions): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.version)
        ++ summon[Encoder[Int]](a.tickerId)
        ++ summon[Encoder[Int]](a.contract.conId)
        ++ summon[Encoder[String]](a.contract.symbol)
        ++ summon[Encoder[SecType]](a.contract.secType)
        ++ summon[Encoder[String]](a.contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](a.contract.strike)
        ++ summon[Encoder[ContractRight]](a.contract.right)
        ++ summon[Encoder[String]](a.contract.multiplier)
        ++ summon[Encoder[String]](a.contract.exchange)
        ++ summon[Encoder[String]](a.contract.primaryExch)
        ++ summon[Encoder[String]](a.contract.currency)
        ++ summon[Encoder[String]](a.contract.localSymbol)
        ++ summon[Encoder[String]](a.contract.tradingClass)
        ++ summon[Encoder[Double]](a.exerciseAction)
        ++ summon[Encoder[Double]](a.exerciseQuantity)
        ++ summon[Encoder[String]](a.account)
        ++ summon[Encoder[Int]](a.`override`)
  end given
}

trait MsgEncoder[A] extends Encoder[A] { self =>

  final def apply(a: A): mutable.Buffer[Byte] =
    this(a)(using IBClient.MAX_VERSION)
  def apply(a: A)(using
      serverVersion: IBClient.ServerVersion
  ): mutable.Buffer[Byte]

  override def contramap[B](f: B => A): Encoder[B] = new MsgEncoder[B] {
    final def apply(a: B)(using
        serverVersion: IBClient.ServerVersion
    ): mutable.Buffer[Byte] = self(f(a))(using serverVersion)
  }
}
