package org.quasigroup.ibclient.request.writers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.IBClient.*
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.Decimal.*
import org.quasigroup.ibclient.types.TypesCodec.given
import org.quasigroup.ibclient.encoder.Encoder.{*, given}
import org.quasigroup.ibclient.request.RequestMsg.PlaceOrder

object PlaceOrderWriter {

  private def writeOrderCondition(cond: OrderCondition): EncoderState =
    for
      _ <- write(cond.conditionType)
      _ <- write(if cond.isConjunctionConnection then "a" else "o")
      _ <- cond match
        case UnknownOrderCondition =>
          writeNothing
        case PriceCondition(_, isMore, price, conId, exchange, triggerMethod) =>
          for
            _ <- write(isMore)
            _ <- write(price)
            _ <- write(conId)
            _ <- write(exchange)
            _ <- write(triggerMethod)
          yield ()
        case TimeCondition(_, isMore, time) =>
          for
            _ <- write(isMore)
            _ <- write(time)
          yield ()
        case MarginCondition(_, isMore, percent) =>
          for
            _ <- write(isMore)
            _ <- write(percent)
          yield ()
        case ExecutionCondition(_, secType, exchange, symbol) =>
          for
            _ <- write(secType)
            _ <- write(exchange)
            _ <- write(symbol)
          yield ()
        case VolumeCondition(_, isMore, volume, conId, exchange) =>
          for
            _ <- write(isMore)
            _ <- write(volume)
            _ <- write(conId)
            _ <- write(exchange)
          yield ()
        case PercentChangeCondition(_, isMore, changePercent, conId, exchange) =>
          for
            _ <- write(isMore)
            _ <- write(changePercent)
            _ <- write(conId)
            _ <- write(exchange)
          yield ()
    yield ()

  def apply(a: PlaceOrder)(using serverVersion: IBClient.ServerVersion): EncoderState = {
    val version = if serverVersion < MIN_SERVER_VER_NOT_HELD then 27 else 45
    for
      _ <- write(a.msgId)
      _ <- if serverVersion < MIN_SERVER_VER_ORDER_CONTAINER then write(version) else writeNothing
      _ <- write(a.id)
      _ <- ContractWriter(a.contract)
      _ <-
        if serverVersion >= MIN_SERVER_VER_SEC_ID_TYPE
        then write(a.contract.secIdType).flatMap(_ => write(a.contract.secId))
        else writeNothing
      _ <- write(a.order.action)
      _ <-
        if serverVersion >= MIN_SERVER_VER_FRACTIONAL_POSITIONS
        then write(a.order.totalQuantity.toString)
        else write(a.order.totalQuantity.value.longValue)
      _ <- write(a.order.orderType)
      _ <-
        if serverVersion < MIN_SERVER_VER_ORDER_COMBO_LEGS_PRICE
        then write(if a.order.lmtPrice == Double.MaxValue then 0 else a.order.lmtPrice)
        else write(if a.order.lmtPrice == Double.MaxValue then "" else a.order.lmtPrice.toString)
      _ <-
        if serverVersion < MIN_SERVER_VER_TRAILING_PERCENT
        then write(if a.order.auxPrice == Double.MaxValue then 0 else a.order.auxPrice)
        else write(if a.order.auxPrice == Double.MaxValue then "" else a.order.auxPrice.toString)
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
            _ <- a.contract.comboLegs.foldLeft(writeNothing) { (buffer, comboLeg) =>
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
            _ <- a.order.orderComboLegs.foldLeft(writeNothing) { (buffer, comboLeg) =>
              buffer.flatMap(_ => write(comboLeg.price))
            }
            _ <- write(a.order.smartComboRoutingFields.size)
            _ <- a.order.smartComboRoutingFields.foldLeft(writeNothing) { (buffer, tagValue) =>
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
      _ <- write(a.order.faFields.map(_.faGroup).getOrElse(""))
      _ <- write(a.order.faFields.map(_.faMethod).getOrElse(Method.None))
      _ <- write(a.order.faFields.map(_.faPercentage).getOrElse(""))
      _ <- if serverVersion < MIN_SERVER_VER_FA_PROFILE_DESUPPORT then write("") else writeNothing
      _ <- write(a.order.modelCode)
      _ <- write(a.order.shortSaleFields.map(_.shortSaleSlot).getOrElse(0))
      _ <- write(a.order.shortSaleFields.map(_.designatedLocation).getOrElse(""))
      _ <- write(a.order.shortSaleFields.map(_.exemptCode).getOrElse(-1))
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
      _ <- write(a.order.boxOrderFields.map(_.startingPrice).getOrElse(Double.MaxValue))
      _ <- write(a.order.boxOrderFields.map(_.stockRefPrice).getOrElse(Double.MaxValue))
      _ <- write(a.order.boxOrderFields.map(_.delta).getOrElse(Double.MaxValue))
      _ <- write(
        if a.order.orderType == Order.Type.VOL then Double.MaxValue
        else a.order.pegToStkOrVolOrderFields.map(_.stockRangeLower).getOrElse(Double.MaxValue)
      )
      _ <- write(
        if a.order.orderType == Order.Type.VOL then Double.MaxValue
        else a.order.pegToStkOrVolOrderFields.map(_.stockRangeUpper).getOrElse(Double.MaxValue)
      )
      _ <- write(a.order.overridePercentageConstraints)
      _ <- write(a.order.volOrderFields.map(_.volatility).getOrElse(Double.MaxValue))
      _ <- write(a.order.volOrderFields.map(_.volatilityType).get)
      _ <- write(a.order.volOrderFields.map(_.deltaNeutralOrderType).getOrElse(Order.Type.None))
      _ <- write(a.order.volOrderFields.map(_.deltaNeutralAuxPrice).getOrElse(Double.MaxValue))
      _ <-
        if a.order.volOrderFields.map(_.deltaNeutralOrderType).getOrElse(Order.Type.None) != Order.Type.None then
          for
            _ <- write(a.order.volOrderFields.map(_.deltaNeutralConId).get)
            _ <- write(a.order.volOrderFields.map(_.deltaNeutralSettlingFirm).get)
            _ <- write(a.order.volOrderFields.map(_.deltaNeutralClearingAccount).get)
            _ <- write(a.order.volOrderFields.map(_.deltaNeutralClearingIntent).get)
            _ <- write(a.order.volOrderFields.map(_.deltaNeutralOpenClose).get)
            _ <- write(a.order.volOrderFields.map(_.deltaNeutralShortSale).get)
            _ <- write(a.order.volOrderFields.map(_.deltaNeutralShortSaleSlot).get)
            _ <- write(
              a.order.volOrderFields.map(_.deltaNeutralDesignatedLocation).get
            )
          yield ()
        else writeNothing
      _ <- write(a.order.volOrderFields.map(_.continuousUpdate).get)
      _ <- write(a.order.volOrderFields.map(_.referencePriceType).get)
      _ <- write(a.order.trailingParams.stopPrice)
      _ <- write(a.order.trailingParams.trailingPercent)
      _ <- write(a.order.scaleFields.scaleInitLevelSize)
      _ <- write(a.order.scaleFields.scaleSubsLevelSize)
      _ <- write(a.order.scaleFields.scalePriceIncrement)
      _ <-
        if a.order.scaleFields.scalePriceIncrement != Double.MaxValue && a.order.scaleFields.scalePriceIncrement > 0.0
        then
          for
            _ <- write(a.order.scaleFields.scalePriceAdjustValue)
            _ <- write(a.order.scaleFields.scalePriceAdjustInterval)
            _ <- write(a.order.scaleFields.scaleProfitOffset)
            _ <- write(a.order.scaleFields.scaleAutoReset)
            _ <- write(a.order.scaleFields.scaleInitPosition)
            _ <- write(a.order.scaleFields.scaleInitFillQty)
            _ <- write(a.order.scaleFields.scaleRandomPercent)
          yield ()
        else writeNothing
      _ <- write(a.order.scaleTable)
      _ <- write(a.order.activeStartTime)
      _ <- write(a.order.activeStopTime)
      _ <- write(a.order.hedgeFields.map(_.`type`).get)
      _ <-
        if a.order.hedgeFields.map(_.`type`).get != HedgeType.None
        then write(a.order.hedgeFields.map(_.value).get)
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

      _ <- write(a.order.algoFields.map(_.strategy).getOrElse(AlgoStrategy.None))
      _ <-
        if a.order.algoFields.map(_.strategy).getOrElse(AlgoStrategy.None) != AlgoStrategy.None then
          for
            _ <- write(a.order.algoFields.map(_.params.size).getOrElse(0))
            _ <- a.order.algoFields
              .map(_.params)
              .getOrElse(Nil)
              .foldLeft(
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
      _ <- write(a.order.algoFields.map(_.id).getOrElse(""))
      _ <- write(a.order.whatIf)
      _ <- write(a.order.orderMiscOptions)
      _ <- write(a.order.solicited)
      _ <-
        if a.order.orderType == Order.Type.PEG_BENCH then
          for
            _ <- write(a.order.pegToBenchFields.map(_.referenceContractId).get)
            _ <- write(
              a.order.pegToBenchFields.map(_.isPeggedChangeAmountDecrease).get
            )
            _ <- write(a.order.pegToBenchFields.map(_.peggedChangeAmount).get)
            _ <- write(a.order.pegToBenchFields.map(_.referenceChangeAmount).get)
            _ <- write(a.order.pegToBenchFields.map(_.referenceExchangeId).get)
          yield ()
        else writeNothing
      _ <- write(a.order.conditionFields.map(_.conditions.size).getOrElse(0))
      _ <-
        a.order.conditionFields.map(_.conditions).getOrElse(Nil).foldLeft(writeNothing) { (buffer, condition) =>
          for
            _ <- buffer
            _ <- writeOrderCondition(condition)
          yield ()
        }

      _ <-
        if a.order.conditionFields.map(_.conditions.nonEmpty).getOrElse(false) then
          for
            _ <- write(a.order.conditionFields.map(_.conditionsIgnoreRth).getOrElse(false))
            _ <- write(a.order.conditionFields.map(_.conditionsCancelOrder).getOrElse(false))
          yield ()
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
          write(a.order.minCompeteSize).flatMap(_ => write(a.order.competeAgainstBestOffset))
        else writeNothing
      _ <-
        if (a.order.orderType == Order.Type.PEG_BEST && a.order.isCompeteAgainstBestOffsetUpToMid) || a.order.orderType == Order.Type.PEG_MID
        then write(a.order.midOffsetAtWhole).flatMap(_ => write(a.order.midOffsetAtHalf))
        else writeNothing
    yield ()
  }
}
