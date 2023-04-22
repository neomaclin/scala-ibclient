package org.quasigroup.ibclient.request

import RequestMsg.*
import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.{*, given}
import org.quasigroup.ibclient.encoder.Encoder
import org.quasigroup.ibclient.encoder.Encoder.{*, given}
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
      (for
        _ <- write(a.msgId)
        _ <- write(a.version)
        _ <- write(a.tickerId)
        _ <- writeC(a.contract)
        _ <- write(a.endDateTime)
        _ <- write(a.barSizeSetting)
        _ <- write(a.durationStr)
        _ <- write(a.useRTH)
        _ <- write(a.whatToShow)
        _ <- write(a.formatDate)
        _ <-
          if a.contract.secType == SecType.BAG then
            for
              _ <- write(a.contract.comboLegs.size)
              _ <- a.contract.comboLegs.foldLeft(writeNothing) {
                (lastWritten, comboLeg) =>
                  for
                    _ <- lastWritten
                    _ <- write(comboLeg.conId)
                    _ <- write(comboLeg.ratio)
                    _ <- write(comboLeg.action)
                    _ <- write(comboLeg.exchange)
                  yield ()
              }
            yield ()
          else writeNothing
        _ <- write(a.keepUpToDate)
        _ <- write(a.chartOptions)
      yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[ReqRealTimeBars] with
    override def apply(a: ReqRealTimeBars): mutable.Buffer[Byte] =
      (for {
        _ <- write(a.msgId)
        _ <- write(a.version)
        _ <- write(a.tickerId)
        _ <- writeC(a.contract)
        _ <- write(a.barSize)
        _ <- write(a.whatToShow)
        _ <- write(a.useRTH)
        _ <- write(a.realTimeBarsOptions)
      } yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[ReqContractDetails] with
    override def apply(a: ReqContractDetails): mutable.Buffer[Byte] =
      (for {
        _ <- write(a.msgId)
        _ <- write(a.version)
        _ <- writeC(a.contract)
        _ <- write(a.contract.includeExpired)
        _ <- write(a.contract.secIdType)
        _ <- write(a.contract.secId)
        _ <- write(a.contract.issuerId)
      } yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[ReqMktDepth] with
    override def apply(a: ReqMktDepth): mutable.Buffer[Byte] =
      (for {
        _ <- write(a.msgId)
        _ <- write(a.version)
        _ <- write(a.tickerId)
        _ <- writeC(a.contract)
        _ <- write(a.numRows)
        _ <- write(a.isSmartDepth)
        _ <- write(a.mktDepthOptions)
      } yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[ReqMktData] with
    override def apply(a: ReqMktData): mutable.Buffer[Byte] =
      (for {
        _ <- write(a.msgId)
        _ <- write(a.version)
        _ <- write(a.tickerId)
        _ <- writeC(a.contract)
        _ <- a.contract.deltaNeutralContract match
          case Some(deltaNeutralContract) =>
            write(true).flatMap(_ => write(deltaNeutralContract))
          case None =>
            write(false)
        _ <- write(a.genericTickList)
        _ <- write(a.snapshot)
        _ <- write(a.regulatorySnapshot)
        _ <- write(a.mktDataOptions)
      } yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[ReqTickByTickData] with
    override def apply(a: ReqTickByTickData): mutable.Buffer[Byte] =
      (for
        _ <- write(a.msgId)
        _ <- write(a.reqId)
        _ <- writeC(a.contract)
        _ <- write(a.tickType)
        _ <- write(a.numberOfTicks)
        _ <- write(a.ignoreSize)
      yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[ReqFundamentalData] with
    override def apply(a: ReqFundamentalData): mutable.Buffer[Byte] =
      (for
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
      yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[CalculateImpliedVolatility] with
    override def apply(a: CalculateImpliedVolatility): mutable.Buffer[Byte] =
      (for
        _ <- write(a.msgId)
        _ <- write(a.version)
        _ <- write(a.reqId)
        _ <- writeC(a.contract)
        _ <- write(a.optionPrice)
        _ <- write(a.underPrice)
        _ <- write(a.impliedVolatilityOptions)
      yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[CalculateOptionPrice] with
    override def apply(a: CalculateOptionPrice): mutable.Buffer[Byte] =
      (for
        _ <- write(a.msgId)
        _ <- write(a.version)
        _ <- write(a.reqId)
        _ <- writeC(a.contract)
        _ <- write(a.volatility)
        _ <- write(a.underPrice)
        _ <- write(a.optionPriceOptions)
      yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[ExerciseOptions] with
    override def apply(a: ExerciseOptions): mutable.Buffer[Byte] =
      (for
        _ <- write(a.msgId)
        _ <- write(a.version)
        _ <- write(a.tickerId)
        _ <- writeC(a.contract)
        _ <- write(a.exerciseAction)
        _ <- write(a.exerciseQuantity)
        _ <- write(a.account)
        _ <- write(a.`override`)
      yield ()).runS(mutable.Buffer.empty).value
  end given

  inline given Encoder[PlaceOrder] with
    override def apply(a: PlaceOrder): mutable.Buffer[Byte] =
      (for
        _ <- write(a.msgId)
        _ <- write(a.id)
        _ <- writeC(a.contract)
        _ <- write(a.contract.secIdType)
        _ <- write(a.contract.secId)
        _ <- write(a.order.action)
        _ <- write(a.order.totalQuantity)
        _ <- write(a.order.orderType)
        _ <- write(a.order.lmtPrice)
        _ <- write(a.order.auxPrice)
        _ <- write(a.order.tif)
        _ <- write(a.order.ocaGroup)
        _ <- write(a.order.account)
        _ <- write(a.order.openClose)
        _ <- write(a.order.origin)
        _ <- write(a.order.orderRef)
        _ <- write(a.order.transmit)
        _ <- write(a.order.parentId)
        _ <- write(a.order.blockOrder)
        _ <- write(a.order.sweepToFill)
        _ <- write(a.order.displaySize)
        _ <- write(a.order.triggerMethod)
        _ <- write(a.order.outsideRth)
        _ <- write(a.order.hidden)
        _ <-
          if a.contract.secType == SecType.BAG then
            for
              _ <- write(a.contract.comboLegs.size)
              _ <- a.contract.comboLegs.foldLeft(writeNothing) {
                (buffer, comboLeg) =>
                  for
                    _ <- buffer
                    _ <- write(comboLeg.conId)
                    _ <- write(comboLeg.ratio)
                    _ <- write(comboLeg.action)
                    _ <- write(comboLeg.exchange)
                    _ <- write(comboLeg.openClose)
                    _ <- write(comboLeg.shortSaleSlot)
                    _ <- write(comboLeg.designatedLocation)
                    _ <- write(comboLeg.exemptCode)
                  yield ()
              }
              _ <- write(a.order.orderComboLegs.size)
              _ <- a.order.orderComboLegs.foldLeft(writeNothing) {
                (buffer, comboLeg) =>
                  buffer.flatMap(_ => write(comboLeg.price))
              }
              _ <- write(a.order.smartComboRoutingParams.size)
              _ <- a.order.smartComboRoutingParams.foldLeft(writeNothing) {
                (buffer, tagValue) =>
                  buffer
                    .flatMap(_ => write(tagValue.tag))
                    .flatMap(_ => write(tagValue.value))
              }
            yield ()
          else writeNothing
        _ <- write("")
        _ <- write(a.order.discretionaryAmt)
        _ <- write(a.order.goodAfterTime)
        _ <- write(a.order.goodTillDate)
        _ <- write(a.order.faGroup)
        _ <- write(a.order.faMethod)
        _ <- write(a.order.faPercentage)
        _ <- write(a.order.faProfile)
        _ <- write(a.order.modelCode)
        _ <- write(a.order.shortSaleSlot)
        _ <- write(a.order.designatedLocation)
        _ <- write(a.order.exemptCode)
        _ <- write(a.order.ocaType)
        _ <- write(a.order.rule80A)
        _ <- write(a.order.settlingFirm)
        _ <- write(a.order.allOrNone)
        _ <- write(a.order.minQty)
        _ <- write(a.order.percentOffset)
        _ <- write(false)
        _ <- write(false)
        _ <- write(Double.MaxValue)
        _ <- write(a.order.auctionStrategy)
        _ <- write(a.order.startingPrice)
        _ <- write(a.order.stockRefPrice)
        _ <- write(a.order.delta)
        _ <- write(
          if a.order.orderType == Order.Type.VOL then Double.MaxValue
          else a.order.stockRangeLower
        )
        _ <- write(
          if a.order.orderType == Order.Type.VOL then Double.MaxValue
          else a.order.stockRangeUpper
        )
        _ <- write(a.order.overridePercentageConstraints)
        _ <- write(a.order.volatility)
        _ <- write(a.order.volatilityType)
        _ <- write(a.order.deltaNeutralOrderType)
        _ <- write(a.order.deltaNeutralAuxPrice)
        _ <-
          if a.order.deltaNeutralOrderType != Order.Type.Ignored then
            for
              _ <- write(a.order.deltaNeutralConId)
              _ <- write(a.order.deltaNeutralSettlingFirm)
              _ <- write(a.order.deltaNeutralClearingAccount)
              _ <- write(a.order.deltaNeutralClearingIntent)
              _ <- write(a.order.deltaNeutralOpenClose)
              _ <- write(a.order.deltaNeutralShortSale)
              _ <- write(a.order.deltaNeutralShortSaleSlot)
              _ <- write(
                a.order.deltaNeutralDesignatedLocation
              )
            yield ()
          else writeNothing
        _ <- write(a.order.continuousUpdate)
        _ <- write(a.order.referencePriceType)
        _ <- write(a.order.trailStopPrice)
        _ <- write(a.order.trailingPercent)
        _ <- write(a.order.scaleInitLevelSize)
        _ <- write(a.order.scaleSubsLevelSize)
        _ <- write(a.order.scalePriceIncrement)
        _ <-
          if a.order.scalePriceIncrement != Double.MaxValue && a.order.scalePriceIncrement > 0.0
          then
            for
              _ <- write(a.order.scalePriceAdjustValue)
              _ <- write(a.order.scalePriceAdjustInterval)
              _ <- write(a.order.scaleProfitOffset)
              _ <- write(a.order.scaleAutoReset)
              _ <- write(a.order.scaleInitPosition)
              _ <- write(a.order.scaleInitFillQty)
              _ <- write(a.order.scaleRandomPercent)
            yield ()
          else writeNothing
        _ <- write(a.order.scaleTable)
        _ <- write(a.order.activeStartTime)
        _ <- write(a.order.activeStopTime)
        _ <- write(a.order.hedgeType)
        _ <-
          if a.order.hedgeType != HedgeType.Ignored
          then write(a.order.hedgeParam)
          else writeNothing
        _ <- write(a.order.optOutSmartRouting)
        _ <- write(a.order.clearingAccount)
        _ <- write(a.order.clearingIntent)
        _ <- write(a.order.notHeld)
        _ <-
          a.contract.deltaNeutralContract match
            case Some(deltaNeutralContract) =>
              write(true).flatMap(_ => write(deltaNeutralContract))
            case None =>
              write(false)

        _ <- write(a.order.algoStrategy)
        _ <-
          if a.order.algoStrategy != AlgoStrategy.Ignored then
            for
              _ <- write(a.order.algoParams.size)
              _ <- a.order.algoParams.foldLeft(
                writeNothing
              ) { (buffer, tagValue) =>
                for
                  _ <- buffer
                  _ <- write(tagValue.tag)
                  _ <- write(tagValue.value)
                yield ()
              }
            yield ()
          else writeNothing
        _ <- write(a.order.algoId)
        _ <- write(a.order.whatIf)
        _ <- write(a.order.orderMiscOptions)
        _ <- write(a.order.solicited)
        _ <-
          if a.order.orderType == Order.Type.PEG_BENCH then
            for
              _ <- write(a.order.referenceContractId)
              _ <- write(
                a.order.isPeggedChangeAmountDecrease
              )
              _ <- write(a.order.peggedChangeAmount)
              _ <- write(a.order.referenceChangeAmount)
              _ <- write(a.order.referenceExchangeId)
            yield ()
          else writeNothing
        _ <- write(a.order.conditions.size)
        _ <-
          a.order.conditions.foldLeft(writeNothing) { (buffer, condition) =>
            for
              _ <- buffer
              _ <- write(condition.conditionType.value)
              _ <- writeRaw(
                writeModifiedUTF(
                  if condition.isConjunctionConnection then "a" else "0"
                )
              )
            yield ()
          }

        _ <-
          if a.order.conditions.size > 0 then
            write(a.order.conditionsIgnoreRth).flatMap(_ =>
              write(a.order.adjustedOrderType)
            )
          else writeNothing
        _ <- write(a.order.adjustedOrderType)
        _ <- write(a.order.triggerPrice)
        _ <- write(a.order.lmtPriceOffset)
        _ <- write(a.order.adjustedStopPrice)
        _ <- write(a.order.adjustedStopLimitPrice)
        _ <- write(a.order.adjustedTrailingAmount)
        _ <- write(a.order.adjustableTrailingUnit)
        _ <- write(a.order.extOperator)
        _ <- write(a.order.softDollarTier.name)
        _ <- write(a.order.softDollarTier.value)
        _ <- write(a.order.cashQty)
        _ <- write(a.order.mifid2DecisionMaker)
        _ <- write(a.order.mifid2DecisionAlgo)
        _ <- write(a.order.mifid2ExecutionTrader)
        _ <- write(a.order.mifid2ExecutionAlgo)
        _ <- write(a.order.dontUseAutoPriceForHedge)
        _ <- write(a.order.isOmsContainer)
        _ <- write(a.order.discretionaryUpToLimitPrice)
        _ <- write(a.order.usePriceMgmtAlgo)
        _ <- write(a.order.duration)
        _ <- write(a.order.postToAts)
        _ <- write(a.order.autoCancelParent)
        _ <- write(a.order.advancedErrorOverride)
        _ <- write(a.order.manualOrderTime)
        _ <-
          if a.contract.exchange == "IBKRATS" then write(a.order.minTradeQty)
          else writeNothing
        _ <-
          if a.order.orderType == Order.Type.PEG_BEST then
            write(a.order.minCompeteSize).flatMap(_ =>
              write(a.order.competeAgainstBestOffset)
            )
          else writeNothing
        _ <-
          if (a.order.orderType == Order.Type.PEG_BEST && a.order.isCompeteAgainstBestOffsetUpToMid) || a.order.orderType == Order.Type.PEG_MID
          then
            write(a.order.midOffsetAtWhole).flatMap(_ =>
              write(a.order.midOffsetAtHalf)
            )
          else writeNothing
      yield ()).runS(mutable.Buffer.empty).value
  end given

  inline def writeC(contract: Contract): EncoderState =
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

end MsgEncoders
