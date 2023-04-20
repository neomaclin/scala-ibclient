package org.quasigroup.ibclient.request

import RequestMsg.*
import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.{*, given}
import org.quasigroup.ibclient.encoder.Encoder
import org.quasigroup.ibclient.encoder.Encoder.given
import cats.MonadThrow
import cats.syntax.try_.*

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
              ++ a.contract.comboLegs.foldLeft(mutable.Buffer.empty) {
                (buffer, comboLeg) =>
                  buffer
                    ++ summon[Encoder[Int]](comboLeg.conId)
                    ++ summon[Encoder[Int]](comboLeg.ratio)
                    ++ summon[Encoder[Action]](comboLeg.action)
                    ++ summon[Encoder[String]](comboLeg.exchange)
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

  inline given Encoder[PlaceOrder] with
    override def apply(a: PlaceOrder): mutable.Buffer[Byte] =
      summon[Encoder[Int]](a.msgId)
        ++ summon[Encoder[Int]](a.id)
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
        ++ summon[Encoder[SecIdType]](a.contract.secIdType)
        ++ summon[Encoder[String]](a.contract.secId)
        ++ summon[Encoder[Action]](a.order.action)
        ++ summon[Encoder[Decimal]](a.order.totalQuantity)
        ++ summon[Encoder[Order.Type]](a.order.orderType)
        ++ summon[Encoder[Double]](a.order.lmtPrice)
        ++ summon[Encoder[Double]](a.order.auxPrice)
        ++ summon[Encoder[TimeInForce]](a.order.tif)
        ++ summon[Encoder[String]](a.order.ocaGroup)
        ++ summon[Encoder[String]](a.order.account)
        ++ summon[Encoder[String]](a.order.openClose)
        ++ summon[Encoder[Int]](a.order.origin)
        ++ summon[Encoder[String]](a.order.orderRef)
        ++ summon[Encoder[Boolean]](a.order.transmit)
        ++ summon[Encoder[Int]](a.order.parentId)
        ++ summon[Encoder[Boolean]](a.order.blockOrder)
        ++ summon[Encoder[Boolean]](a.order.sweepToFill)
        ++ summon[Encoder[Int]](a.order.displaySize)
        ++ summon[Encoder[TriggerMethod]](a.order.triggerMethod)
        ++ summon[Encoder[Boolean]](a.order.outsideRth)
        ++ summon[Encoder[Boolean]](a.order.hidden)
        ++ (
          if a.contract.secType == SecType.BAG then
            summon[Encoder[Int]](a.contract.comboLegs.size)
              ++ a.contract.comboLegs.foldLeft(mutable.Buffer.empty) {
                (buffer, comboLeg) =>
                  buffer
                    ++ summon[Encoder[Int]](comboLeg.conId)
                    ++ summon[Encoder[Int]](comboLeg.ratio)
                    ++ summon[Encoder[Action]](comboLeg.action)
                    ++ summon[Encoder[String]](comboLeg.exchange)
                    ++ summon[Encoder[Int]](comboLeg.openClose)
                    ++ summon[Encoder[Int]](comboLeg.shortSaleSlot)
                    ++ summon[Encoder[String]](comboLeg.designatedLocation)
                    ++ summon[Encoder[Int]](comboLeg.exemptCode)
              }
              ++ summon[Encoder[Int]](a.order.orderComboLegs.size)
              ++ a.order.orderComboLegs.foldLeft(mutable.Buffer.empty) {
                (buffer, comboLeg) =>
                  buffer ++ summon[Encoder[Double]](comboLeg.price)
              }
              ++ summon[Encoder[Int]](a.order.smartComboRoutingParams.size)
              ++ a.order.smartComboRoutingParams.foldLeft(
                mutable.Buffer.empty
              ) { (buffer, tagValue) =>
                buffer
                  ++ summon[Encoder[String]](tagValue.tag)
                  ++ summon[Encoder[String]](tagValue.value)
              }
          else mutable.Buffer.empty
        )
        ++ summon[Encoder[String]]("")
        ++ summon[Encoder[Double]](a.order.discretionaryAmt)
        ++ summon[Encoder[String]](a.order.goodAfterTime)
        ++ summon[Encoder[String]](a.order.goodTillDate)
        ++ summon[Encoder[String]](a.order.faGroup)
        ++ summon[Encoder[Method]](a.order.faMethod)
        ++ summon[Encoder[String]](a.order.faPercentage)
        ++ summon[Encoder[String]](a.order.faProfile)
        ++ summon[Encoder[String]](a.order.modelCode)
        ++ summon[Encoder[Int]](a.order.shortSaleSlot)
        ++ summon[Encoder[String]](a.order.designatedLocation)
        ++ summon[Encoder[Int]](a.order.exemptCode)
        ++ summon[Encoder[OcaType]](a.order.ocaType)
        ++ summon[Encoder[Rule80A]](a.order.rule80A)
        ++ summon[Encoder[String]](a.order.settlingFirm)
        ++ summon[Encoder[Boolean]](a.order.allOrNone)
        ++ summon[Encoder[Int]](a.order.minQty)
        ++ summon[Encoder[Double]](a.order.percentOffset)
        ++ summon[Encoder[Boolean]](false)
        ++ summon[Encoder[Boolean]](false)
        ++ summon[Encoder[Double]](Double.MaxValue)
        ++ summon[Encoder[AuctionStrategy]](a.order.auctionStrategy)
        ++ summon[Encoder[Double]](a.order.startingPrice)
        ++ summon[Encoder[Double]](a.order.stockRefPrice)
        ++ summon[Encoder[Double]](a.order.delta)
        ++ summon[Encoder[Double]](
          if a.order.orderType == Order.Type.VOL then Double.MaxValue
          else a.order.stockRangeLower
        )
        ++ summon[Encoder[Double]](
          if a.order.orderType == Order.Type.VOL then Double.MaxValue
          else a.order.stockRangeUpper
        )
        ++ summon[Encoder[Boolean]](a.order.overridePercentageConstraints)
        ++ summon[Encoder[Double]](a.order.volatility)
        ++ summon[Encoder[VolatilityType]](a.order.volatilityType)
        ++ summon[Encoder[Order.Type]](a.order.deltaNeutralOrderType)
        ++ summon[Encoder[Double]](a.order.deltaNeutralAuxPrice)
        ++ (if a.order.deltaNeutralOrderType != Order.Type.Ignored then
              summon[Encoder[Int]](a.order.deltaNeutralConId)
                ++ summon[Encoder[String]](a.order.deltaNeutralSettlingFirm)
                ++ summon[Encoder[String]](a.order.deltaNeutralClearingAccount)
                ++ summon[Encoder[String]](a.order.deltaNeutralClearingIntent)
                ++ summon[Encoder[String]](a.order.deltaNeutralOpenClose)
                ++ summon[Encoder[Boolean]](a.order.deltaNeutralShortSale)
                ++ summon[Encoder[Double]](a.order.deltaNeutralShortSaleSlot)
                ++ summon[Encoder[String]](
                  a.order.deltaNeutralDesignatedLocation
                )
            else mutable.Buffer.empty)
        ++ summon[Encoder[Int]](a.order.continuousUpdate)
        ++ summon[Encoder[ReferencePriceType]](a.order.referencePriceType)
        ++ summon[Encoder[Double]](a.order.trailStopPrice)
        ++ summon[Encoder[Double]](a.order.trailingPercent)
        ++ summon[Encoder[Int]](a.order.scaleInitLevelSize)
        ++ summon[Encoder[Int]](a.order.scaleSubsLevelSize)
        ++ summon[Encoder[Double]](a.order.scalePriceIncrement)
        ++ (if a.order.scalePriceIncrement != Double.MaxValue && a.order.scalePriceIncrement > 0.0
            then
              summon[Encoder[Double]](a.order.scalePriceAdjustValue)
                ++ summon[Encoder[Double]](a.order.scalePriceAdjustInterval)
                ++ summon[Encoder[Double]](a.order.scaleProfitOffset)
                ++ summon[Encoder[Boolean]](a.order.scaleAutoReset)
                ++ summon[Encoder[Int]](a.order.scaleInitPosition)
                ++ summon[Encoder[Int]](a.order.scaleInitFillQty)
                ++ summon[Encoder[Boolean]](a.order.scaleRandomPercent)
            else mutable.Buffer.empty)
        ++ summon[Encoder[String]](a.order.scaleTable)
        ++ summon[Encoder[String]](a.order.activeStartTime)
        ++ summon[Encoder[String]](a.order.activeStopTime)
        ++ summon[Encoder[HedgeType]](a.order.hedgeType)
        ++ (if a.order.hedgeType != HedgeType.Ignored
            then summon[Encoder[String]](a.order.hedgeParam)
            else mutable.Buffer.empty)
        ++ summon[Encoder[Boolean]](a.order.optOutSmartRouting)
        ++ summon[Encoder[String]](a.order.clearingAccount)
        ++ summon[Encoder[String]](a.order.clearingIntent)
        ++ summon[Encoder[Boolean]](a.order.notHeld)
        ++ (
          a.contract.deltaNeutralContract match
            case Some(deltaNeutralContract) =>
              summon[Encoder[Boolean]](true)
                ++ summon[Encoder[DeltaNeutralContract]](deltaNeutralContract)
            case None => summon[Encoder[Boolean]](false)
        )
        ++ summon[Encoder[AlgoStrategy]](a.order.algoStrategy)
        ++ (if a.order.algoStrategy != AlgoStrategy.Ignored then
              summon[Encoder[Int]](a.order.algoParams.size)
                ++ a.order.algoParams.foldLeft(
                  mutable.Buffer.empty
                ) { (buffer, tagValue) =>
                  buffer
                    ++ summon[Encoder[String]](tagValue.tag)
                    ++ summon[Encoder[String]](tagValue.value)
                }
            else mutable.Buffer.empty)
        ++ summon[Encoder[String]](a.order.algoId)
        ++ summon[Encoder[Boolean]](a.order.whatIf)
        ++ summon[Encoder[List[TagValue]]](a.order.orderMiscOptions)
        ++ summon[Encoder[Boolean]](a.order.solicited)
        ++ (if a.order.orderType == Order.Type.PEG_BENCH then
              summon[Encoder[Int]](a.order.referenceContractId)
                ++ summon[Encoder[Boolean]](
                  a.order.isPeggedChangeAmountDecrease
                )
                ++ summon[Encoder[Double]](a.order.peggedChangeAmount)
                ++ summon[Encoder[Double]](a.order.referenceChangeAmount)
                ++ summon[Encoder[String]](a.order.referenceExchangeId)
            else mutable.Buffer.empty)
        ++ summon[Encoder[Int]](a.order.conditions.size)
        ++ (
          a.order.conditions.foldLeft(mutable.Buffer.empty) {
            (buffer, condition) =>
              buffer
                ++ summon[Encoder[Int]](condition.conditionType.value)
                ++ writeModifiedUTF(
                  if condition.isConjunctionConnection then "a" else "0"
                )
          }
        )
        ++ (if a.order.conditions.size > 0 then
              summon[Encoder[Boolean]](a.order.conditionsIgnoreRth)
                ++ summon[Encoder[Order.Type]](a.order.adjustedOrderType)
            else mutable.Buffer.empty)
        ++ summon[Encoder[Order.Type]](a.order.adjustedOrderType)
        ++ summon[Encoder[Double]](a.order.triggerPrice)
        ++ summon[Encoder[Double]](a.order.lmtPriceOffset)
        ++ summon[Encoder[Double]](a.order.adjustedStopPrice)
        ++ summon[Encoder[Double]](a.order.adjustedStopLimitPrice)
        ++ summon[Encoder[Double]](a.order.adjustedTrailingAmount)
        ++ summon[Encoder[Int]](a.order.adjustableTrailingUnit)
        ++ summon[Encoder[String]](a.order.extOperator)
        ++ summon[Encoder[String]](a.order.softDollarTier.name)
        ++ summon[Encoder[String]](a.order.softDollarTier.value)
        ++ summon[Encoder[Double]](a.order.cashQty)
        ++ summon[Encoder[String]](a.order.mifid2DecisionMaker)
        ++ summon[Encoder[String]](a.order.mifid2DecisionAlgo)
        ++ summon[Encoder[String]](a.order.mifid2ExecutionTrader)
        ++ summon[Encoder[String]](a.order.mifid2ExecutionAlgo)
        ++ summon[Encoder[Boolean]](a.order.dontUseAutoPriceForHedge)
        ++ summon[Encoder[Boolean]](a.order.isOmsContainer)
        ++ summon[Encoder[Boolean]](a.order.discretionaryUpToLimitPrice)
        ++ summon[Encoder[UsePriceMgmtAlgo]](a.order.usePriceMgmtAlgo)
        ++ summon[Encoder[Int]](a.order.duration)
        ++ summon[Encoder[Int]](a.order.postToAts)
        ++ summon[Encoder[Boolean]](a.order.autoCancelParent)
        ++ summon[Encoder[String]](a.order.advancedErrorOverride)
        ++ summon[Encoder[String]](a.order.manualOrderTime)
        ++ (if a.contract.exchange == "IBKRATS" then
              summon[Encoder[Int]](a.order.minTradeQty)
            else mutable.Buffer.empty)
        ++ (
          if a.order.orderType == Order.Type.PEG_BEST then
            summon[Encoder[Double]](a.order.minCompeteSize)
              ++ summon[Encoder[Double]](a.order.competeAgainstBestOffset)
          else mutable.Buffer.empty
        )
        ++ (
          if (a.order.orderType == Order.Type.PEG_BEST && a.order.isCompeteAgainstBestOffsetUpToMid) || a.order.orderType == Order.Type.PEG_MID
          then
            summon[Encoder[Double]](a.order.midOffsetAtWhole)
              ++ summon[Encoder[Double]](a.order.midOffsetAtHalf)
          else mutable.Buffer.empty
        )

    // ++ summon[Encoder[AlgoStrategy]](a.order.randomizeSize)

    // ++ summon[Encoder[AlgoStrategy]](a.order.randomizePrice)

  end given

end MsgEncoders

//trait MsgEncoder[A] extends Encoder[A] { self =>
//
//  final def apply(a: A): mutable.Buffer[Byte] =
//    this(a)(using IBClient.MAX_VERSION)
//  def apply(a: A)(using
//      serverVersion: IBClient.ServerVersion
//  ): mutable.Buffer[Byte]
//
//  override def contramap[B](f: B => A): Encoder[B] = new MsgEncoder[B] {
//    final def apply(a: B)(using
//        serverVersion: IBClient.ServerVersion
//    ): mutable.Buffer[Byte] = self(f(a))(using serverVersion)
//  }
//}
