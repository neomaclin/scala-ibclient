package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.IBClient.*
import org.quasigroup.ibclient.decoder.Decoder.*
import org.quasigroup.ibclient.response.ResponseMsg.*
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given

import cats.data.State
import cats.syntax.option.*

object OrderReader {

  private def readConditions(using serverVersion: IBClient.ServerVersion): DecoderState[Option[Order.ConditionParams]] =
    if serverVersion >= MIN_SERVER_VER_PEGGED_TO_BENCHMARK then
      for
        nConditions <- read[Int]
        conditions <- (0 until nConditions).foldLeft(readNothing(List.empty[OrderCondition])) { (state, idx) =>
          for
            list <- state
            orderConditionType <- read[OrderConditionType]
            isConjunctionConnection <- read[String].map(_ == "a")
            orderCondition <-
              orderConditionType match
                case OrderConditionType.Unknown =>
                  readNothing(UnknownOrderCondition)
                case OrderConditionType.Price =>
                  for
                    isMore <- read[Boolean]
                    price <- read[Double]
                    conId <- read[Int]
                    exchange <- read[String]
                    triggerMethod <- read[TriggerMethod]
                  yield PriceCondition(isConjunctionConnection, isMore, price, conId, exchange, triggerMethod)
                case OrderConditionType.Time =>
                  for
                    isMore <- read[Boolean]
                    time <- read[String]
                  yield TimeCondition(isConjunctionConnection, isMore, time)
                case OrderConditionType.Margin =>
                  for
                    isMore <- read[Boolean]
                    percent <- read[Int]
                  yield MarginCondition(isConjunctionConnection, isMore, percent)
                case OrderConditionType.Execution =>
                  for
                    secType <- read[SecType]
                    exchange <- read[String]
                    symbol <- read[String]
                  yield ExecutionCondition(isConjunctionConnection, secType, exchange, symbol)
                case OrderConditionType.Volume =>
                  for
                    isMore <- read[Boolean]
                    volume <- read[Int]
                    conId <- read[Int]
                    exchange <- read[String]
                  yield VolumeCondition(isConjunctionConnection, isMore, volume, conId, exchange)
                case OrderConditionType.PercentChange =>
                  for
                    isMore <- read[Boolean]
                    changePercent <- read[Double]
                    conId <- read[Int]
                    exchange <- read[String]
                  yield PercentChangeCondition(isConjunctionConnection, isMore, changePercent, conId, exchange)
          yield orderCondition :: list
        }
        conditionsIgnoreRth <- read[Boolean]
        conditionsCancelOrder <- read[Boolean]
      yield Order.ConditionParams(conditions.reverse, conditionsIgnoreRth, conditionsCancelOrder).some
    else readNothing(None)

  private def readOrderStateCommission(
      version: Int
  )(using serverVersion: IBClient.ServerVersion): DecoderState[Order.State] =
    if version >= 16 then
      for
        orderStatus <- read[Order.Status]
        rightServerVersin = (serverVersion >= MIN_SERVER_VER_WHAT_IF_EXT_FIELDS)
        initMarginBefore <- if rightServerVersin then read[String] else readNothing("")
        maintMarginBefore <- if rightServerVersin then read[String] else readNothing("")
        equityWithLoanBefore <- if rightServerVersin then read[String] else readNothing("")
        initMarginChange <- if rightServerVersin then read[String] else readNothing("")
        maintMarginChange <- if rightServerVersin then read[String] else readNothing("")
        equityWithLoanChange <- if rightServerVersin then read[String] else readNothing("")
        initMarginAfter <- read[String]
        maintMarginAfter <- read[String]
        equityWithLoanAfter <- read[String]
        commission <- readDoubleMax
        minCommission <- readDoubleMax
        maxCommission <- readDoubleMax
        commissionCurrency <- read[String]
        warningText <- read[String]
      yield Order.State(
        status = orderStatus,
        initMarginBefore = initMarginBefore,
        maintMarginBefore = maintMarginBefore,
        equityWithLoanBefore = equityWithLoanBefore,
        initMarginChange = initMarginChange,
        maintMarginChange = maintMarginChange,
        equityWithLoanChange = equityWithLoanChange,
        initMarginAfter = initMarginAfter,
        maintMarginAfter = maintMarginAfter,
        equityWithLoanAfter = equityWithLoanAfter,
        commission = commission,
        minCommission = minCommission,
        maxCommission = maxCommission,
        commissionCurrency = commissionCurrency,
        warningText = warningText
        // completedTime: String = "",
        // completedStatus: Status  = Order.Status.Unknown,
      )
    else readNothing(Order.State())

  private def readPegToBenchParams(
      orderType: Order.Type
  )(using serverVersion: IBClient.ServerVersion): DecoderState[Option[Order.PegToBenchParams]] =
    if serverVersion >= MIN_SERVER_VER_PEGGED_TO_BENCHMARK then
      if orderType == Order.Type.PEG_BENCH then read[Order.PegToBenchParams].map(_.some)
      else readNothing(None)
    else readNothing(None)

  private def readAlgoParams(version: Int): DecoderState[Option[Order.AlgoParams]] =
    if version >= 21 then
      for
        algoStrategy <- read[AlgoStrategy]
        params <-
          if algoStrategy == AlgoStrategy.None then readNothing(Nil)
          else
            for
              algoParamsCount <- read[Int]
              algoParams <-
                (0 until algoParamsCount)
                  .foldLeft(readNothing(List.empty[TagValue])) { (state, idx) =>
                    for
                      list <- state
                      tagValue <- read[TagValue]
                    yield tagValue :: list
                  }
            yield algoParams
      yield Order.AlgoParams(algoStrategy, params.reverse, "").some
    else readNothing(None)

  private def readDeltaNeutral(version: Int): DecoderState[Option[DeltaNeutralContract]] =
    if version >= 20 then
      for
        shouldRead <- read[Boolean]
        result <- if shouldRead then read[DeltaNeutralContract].map(_.some) else readNothing(None)
      yield result
    else readNothing(None)

  private def readHedgeParams(version: Int): DecoderState[Option[Order.HedgeParams]] =
    if version >= 14 then
      for
        hedgeType <- read[HedgeType]
        hedgeParam <- if hedgeType == HedgeType.None then readNothing("") else read[String]
      yield Order.HedgeParams(hedgeType, hedgeParam).some
    else readNothing(None)

  private def readScaleOrderParams(version: Int): DecoderState[Order.ScaleOrderParams] =
    for
      v1 <- if version >= 15 then readIntMax else readNothing(Int.MaxValue)
      v2 <- if version >= 15 then readIntMax else readNothing(Int.MaxValue)
      scalePriceIncrement <- if version >= 15 then readDoubleMax else readNothing(Double.MaxValue)
      shouldRead = (version >= 28 && scalePriceIncrement > 0.0 && scalePriceIncrement != Double.MaxValue)
      scalePriceAdjustValue <- if shouldRead then readDoubleMax else readNothing(Double.MaxValue)
      scalePriceAdjustInterval <- if shouldRead then readIntMax else readNothing(Int.MaxValue)
      scaleProfitOffset <- if shouldRead then readDoubleMax else readNothing(Double.MaxValue)
      scaleAutoReset <- if shouldRead then read[Boolean] else readNothing(false)
      scaleInitPosition <- if shouldRead then readIntMax else readNothing(Int.MaxValue)
      scaleInitFillQty <- if shouldRead then readIntMax else readNothing(Int.MaxValue)
      scaleRandomPercent <- if shouldRead then read[Boolean] else readNothing(false)
    yield
      val scaleInitLevelSize = if version >= 20 then v1 else v2
      val scaleSubsLevelSize = if version >= 20 then v2 else Int.MaxValue
      Order.ScaleOrderParams(
        scaleInitLevelSize,
        scaleSubsLevelSize,
        scalePriceIncrement,
        scalePriceAdjustValue,
        scalePriceAdjustInterval,
        scaleProfitOffset,
        scaleAutoReset,
        scaleInitPosition,
        scaleInitFillQty,
        scaleRandomPercent
      )

  private case class ComboLegsParams(
      comboLegsDescrip: String = "",
      contractComboLegs: List[ComboLeg] = Nil,
      orderComboLegs: List[Order.ComboLeg] = Nil
  )

  private def readComboLegs(version: Int): DecoderState[ComboLegsParams] =
    for
      comboLegsDescrip <- if version >= 14 then read[String] else readNothing("")
      params <-
        if version >= 29 then
          for
            contractComboLegsCount <- read[Int]
            contractComboLegs <-
              (0 until contractComboLegsCount)
                .foldLeft(readNothing(List.empty[ComboLeg])) { (state, idx) =>
                  for
                    list <- state
                    conId <- read[Int]
                    ratio <- read[Int]
                    action <- read[Action]
                    exchange <- read[String]
                    openClose <- read[ComboLeg.OpenClose]
                    shortSaleSlot <- read[Int]
                    designatedLocation <- read[String]
                    exemptCode <- read[Int]
                  yield ComboLeg(
                    conId,
                    ratio,
                    action,
                    exchange,
                    openClose,
                    shortSaleSlot,
                    designatedLocation,
                    exemptCode
                  ) :: list
                }
            orderComboLegsCount <- read[Int]
            orderComboLegs <-
              (0 until orderComboLegsCount)
                .foldLeft(readNothing(List.empty[Order.ComboLeg])) { (state, idx) =>
                  for
                    list <- state
                    price <- readDoubleMax
                  yield Order.ComboLeg(price) :: list
                }
          yield ComboLegsParams(comboLegsDescrip, contractComboLegs.reverse, orderComboLegs.reverse)
        else readNothing(ComboLegsParams(comboLegsDescrip))
    yield params

  private def readShortSaleParams(
      version: Int
  )(using serverVersion: IBClient.ServerVersion): DecoderState[Option[Order.ShortSaleParams]] =
    if version >= 9 then
      for
        shortSaleSlot <- read[Int]
        designatedLocation <- read[String]
        exemptCode <-
          if serverVersion == IBClient.ServerVersion(51) then read[Int].map(_ => -1)
          else if version >= 23 then read[Int]
          else readNothing(-1)
      yield Order.ShortSaleParams(shortSaleSlot, designatedLocation, exemptCode).some
    else readNothing(None)

  private def readBoxOrdrParams(version: Int): DecoderState[Option[Order.BoxOrderParams]] =
    if version >= 9 then read[Order.BoxOrderParams].map(_.some) else readNothing(None)

  private def readPegToStkOrVolOrderParams(version: Int): DecoderState[Option[Order.PegToStkOrVolOrderParams]] =
    if version >= 9 then read[Order.PegToStkOrVolOrderParams].map(_.some) else readNothing(None)

  private def readSmartComboRoutingParams(version: Int): DecoderState[List[TagValue]] =
    if version >= 26 then
      for
        smartComboRoutingParamsCount <- read[Int]
        entries <- (0 until smartComboRoutingParamsCount).foldLeft(readNothing(List.empty[TagValue])) { (state, idx) =>
          for
            list <- state
            tag <- read[String]
            value <- read[String]
          yield TagValue(tag, value) :: list
        }
      yield entries
    else readNothing(Nil)

  private def readBasisPoints(version: Int): DecoderState[Option[Order.BasisPoints]] =
    if version >= 14 then read[Order.BasisPoints].map(_.some) else readNothing(None)

  private def readFAParams(
      version: Int
  )(using serverVersion: IBClient.ServerVersion): DecoderState[Option[Order.FAParams]] =
    if version >= 7 then
      for
        faGroup <- read[String]
        faMethod <- read[Method]
        faPercentage <- read[String]
        faProfile <- if serverVersion < MIN_SERVER_VER_FA_PROFILE_DESUPPORT then read[String] else readNothing("")
      yield Some(Order.FAParams(faGroup, faMethod, faPercentage))
    else readNothing(None)

  private def readVolOrderParams(
      version: Int,
      readOpenOrderAttribs: Boolean
  ): DecoderState[Option[Order.VolOrderParams]] =
    if version >= 11 then
      for
        volatility <- readDoubleMax
        volatilityType <- read[VolatilityType]
        version11 = (version == 11)
        deltaNeutralOrderType <-
          if version11 then read[Int].map(value => if value == 0 then Order.Type.None else Order.Type.MKT)
          else read[Order.Type]
        deltaNeutralAuxPrice <- if version11 then readNothing(Double.MaxValue) else readDoubleMax
        v27Newer = (version >= 27 && deltaNeutralOrderType != Order.Type.None)
        deltaNeutralConId <- if v27Newer then read[Int] else readNothing(-1)
        deltaNeutralSettlingFirm <- if v27Newer && readOpenOrderAttribs then read[String] else readNothing("")
        deltaNeutralClearingAccount <- if v27Newer && readOpenOrderAttribs then read[String] else readNothing("")
        deltaNeutralClearingIntent <- if v27Newer && readOpenOrderAttribs then read[String] else readNothing("")
        v31Newer = (version >= 31 && deltaNeutralOrderType != Order.Type.None)
        deltaNeutralOpenClose <- if v31Newer && readOpenOrderAttribs then read[String] else readNothing("")
        deltaNeutralShortSale <- if v31Newer then read[Boolean] else readNothing(false)
        deltaNeutralShortSaleSlot <- if v31Newer then read[Int] else readNothing(-1)
        deltaNeutralDesignatedLocation <- if v31Newer then read[String] else readNothing("")
        continuousUpdate <- read[Int]
        // if (m_serverVersion == 26) { // never happen for later version of tws api
        //     m_order.stockRangeLower(m_eDecoder.readDouble());
        //     m_order.stockRangeUpper(m_eDecoder.readDouble());
        // }
        referencePriceType <- read[ReferencePriceType]
      yield Some(
        Order.VolOrderParams(
          volatility,
          volatilityType,
          continuousUpdate,
          referencePriceType,
          deltaNeutralOrderType,
          deltaNeutralAuxPrice,
          deltaNeutralConId,
          deltaNeutralSettlingFirm,
          deltaNeutralClearingAccount,
          deltaNeutralClearingIntent,
          deltaNeutralOpenClose,
          deltaNeutralShortSale,
          deltaNeutralShortSaleSlot,
          deltaNeutralDesignatedLocation
        )
      )
    else readNothing(None)

  private def readContract(version: Int): DecoderState[Contract] =
    for
      conid <- if version >= 17 then read[Int] else readNothing(-1)
      symbol <- read[String]
      secType <- read[SecType]
      lastTradeDateOrContractMonth <- read[String]
      strike <- read[Double]
      right <- read[ContractRight]
      multiplier <- if (version >= 32) then read[String] else readNothing("")
      exchange <- read[String]
      currency <- read[String]
      localSymbol <- if version >= 2 then read[String] else readNothing("")
      tradingClass <- if version >= 32 then read[String] else readNothing("")
    yield Contract(
      conId = conid,
      symbol = symbol,
      secType = secType,
      lastTradeDateOrContractMonth = lastTradeDateOrContractMonth,
      strike = strike,
      right = right,
      exchange = exchange,
      currency = currency,
      multiplier = multiplier,
      localSymbol = localSymbol,
      tradingClass = tradingClass
    )

  def processOpenOrder(using serverVersion: IBClient.ServerVersion): DecoderState[OpenOrder] =
    for
      version <- read[Int]
      orderId <- read[Int]
      contract <- readContract(version)
      // read order fields
      action <- read[Action]
      totalQuantity <- read[Decimal]
      orderType <- read[Order.Type]
      lmtPrice <- if version < 29 then read[Double] else readDoubleMax // check version
      auxPrice <- if version < 30 then read[Double] else readDoubleMax // check version
      tif <- read[TimeInForce]
      ocaGroup <- read[String]
      account <- read[String]
      openClose <- read[String]
      origin <- read[Int]
      orderRef <- read[String]
      clientId <- if version >= 3 then read[Int] else readNothing(-1)
      permId <- if version >= 4 then read[Int] else readNothing(-1)
      outsideRth <- if version >= 4 then read[Boolean] else readNothing(false)
      hidden <- if version >= 4 then read[Int].map(_ == 1) else readNothing(false)
      discretionaryAmt <- if version >= 4 then read[Double] else readNothing(Double.MaxValue)
      goodAfterTime <- if version >= 5 then read[String] else readNothing("")
      _ <- if version >= 6 then read[String] else readNothing("") // eOrderDecoder.skipSharesAllocation();
      faFields <- readFAParams(version)
      modelCode <- read[String]
      goodTillDate <- if version >= 8 then read[String] else readNothing("")
      rule80A <- if version >= 9 then read[Rule80A] else readNothing(Rule80A.None)
      percentOffset <- if version >= 9 then read[Double] else readNothing(Double.MaxValue)
      settlingFirm <- if version >= 9 then read[String] else readNothing("")
      shortSaleFields <- readShortSaleParams(version)
      auctionStrategy <- if version >= 9 then read[AuctionStrategy] else readNothing(AuctionStrategy.None)
      boxOrderFields <- readBoxOrdrParams(version)
      pegToStkOrVolOrderFields <- readPegToStkOrVolOrderParams(version)
      displaySize <- if version >= 9 then read[Int] else readNothing(Int.MaxValue)
      // skipping eOrderDecoder.readOldStyleOutsideRth();
      blockOrder <- if version >= 9 then read[Boolean] else readNothing(false)
      sweepToFill <- if version >= 9 then read[Boolean] else readNothing(false)
      allOrNone <- if version >= 9 then read[Boolean] else readNothing(false)
      minQty <- if version >= 9 then read[Int] else readNothing(Int.MaxValue)
      ocaType <- if version >= 9 then read[OcaType] else readNothing(OcaType.None)
      _ <-
        if version >= 9 then read[Int] else readNothing(0) // eOrderDecoder.readETradeOnly(); // skip deprecated field
      _ <- if version >= 9 then read[Int] else readNothing(0) // eOrderDecoder.readFirmQuoteOnly();
      _ <- if version >= 9 then read[Int] else readNothing(0) //  eOrderDecoder.readNbboPriceCap();
      parentId <- if version >= 10 then read[Int] else readNothing(-1)
      triggerMethod <- if version >= 10 then read[TriggerMethod] else readNothing(TriggerMethod.Default)
      volOrderFields <- readVolOrderParams(version, true)
      _ <- if version >= 13 then readDoubleMax else readNothing(Double.MaxValue)
      trailingPercent <- if version >= 30 then readDoubleMax else readNothing(Double.MaxValue)
      basisPoints <- readBasisPoints(version)
      comboLegFields <- readComboLegs(version)
      smartComboRoutingFields <- readSmartComboRoutingParams(version)
      scaleFields <- readScaleOrderParams(version)
      hedgeFields <- readHedgeParams(version)
      optOutSmartRouting <- if version >= 25 then read[Boolean] else readNothing(false)
      clearingAccount <- if version >= 19 then read[String] else readNothing("")
      clearingIntent <- if version >= 19 then read[String] else readNothing("")
      notHeld <- if version >= 22 then read[Boolean] else readNothing(false)
      contractDeltaNeutral <- readDeltaNeutral(version)
      algoFields <- readAlgoParams(version)
      solicited <- if version >= 33 then read[Boolean] else readNothing(false)
      whatIf <- if version >= 16 then read[Boolean] else readNothing(false)
      orderState <- readOrderStateCommission(version)
      randomizeSize <- if version >= 34 then read[Boolean] else readNothing(false)
      randomizePrice <- if version >= 34 then read[Boolean] else readNothing(false)
      pegToBenchFields <- readPegToBenchParams(orderType)
      conditionsField <- readConditions
      adjustedOrderType <- read[Order.Type]
      triggerPrice <- readDoubleMax
      trailStopPrice <- readDoubleMax
      lmtPriceOffset <- readDoubleMax
      adjustedStopPrice <- readDoubleMax
      adjustedStopLimitPrice <- readDoubleMax
      adjustedTrailingAmount <- readDoubleMax
      adjustableTrailingUnit <- read[Int]
      softDollarTier <- read[SoftDollarTier]
      cashQty <- readDoubleMax
      dontUseAutoPriceForHedge <- read[Boolean]
      isOmsContainer <- if version >= 33 then read[Boolean] else readNothing(false)
      discretionaryUpToLimitPrice <- read[Boolean]
      usePriceMgmtAlgo <- read[UsePriceMgmtAlgo]
      duration <- read[Int]
      postToAts <- readIntMax
      autoCancelParent <- read[Boolean]
      minTradeQty <- readIntMax
      minCompeteSize <- readIntMax
      competeAgainstBestOffset <- readDoubleMax
      midOffsetAtWhole <- readDoubleMax
      midOffsetAtHalf <- readDoubleMax
    yield {
      val order =
        Order(
          orderId = orderId,
          action = action,
          totalQuantity = totalQuantity,
          orderType = orderType,
          lmtPrice = lmtPrice, // check version
          auxPrice = auxPrice, // check version
          tif = tif,
          ocaGroup = ocaGroup,
          account = account,
          openClose = openClose,
          origin = origin,
          orderRef = orderRef,
          clientId = clientId,
          permId = permId,
          outsideRth = outsideRth,
          hidden = hidden,
          discretionaryAmt = discretionaryAmt,
          goodAfterTime = goodAfterTime,
          faFields = faFields,
          modelCode = modelCode,
          goodTillDate = goodTillDate,
          rule80A = rule80A,
          percentOffset = percentOffset,
          settlingFirm = settlingFirm,
          shortSaleFields = shortSaleFields,
          auctionStrategy = auctionStrategy,
          boxOrderFields = boxOrderFields,
          pegToStkOrVolOrderFields = pegToStkOrVolOrderFields,
          displaySize = displaySize,
          blockOrder = blockOrder,
          sweepToFill = sweepToFill,
          allOrNone = allOrNone,
          minQty = minQty,
          ocaType = ocaType,
          parentId = parentId,
          triggerMethod = triggerMethod,
          volOrderFields = volOrderFields,
          basisPoints = basisPoints,
          orderComboLegs = comboLegFields.orderComboLegs,
          smartComboRoutingFields = smartComboRoutingFields,
          scaleFields = scaleFields,
          hedgeFields = hedgeFields,
          optOutSmartRouting = optOutSmartRouting,
          clearingAccount = clearingAccount,
          clearingIntent = clearingIntent,
          notHeld = notHeld,
          algoFields = algoFields,
          solicited = solicited,
          whatIf = whatIf,
          randomizeSize = randomizeSize,
          randomizePrice = randomizePrice,
          pegToBenchFields = pegToBenchFields,
          conditionFields = conditionsField,
          adjustedOrderType = adjustedOrderType,
          triggerPrice = triggerPrice,
          trailStopPrice = trailStopPrice,
          trailingPercent = trailingPercent,
          lmtPriceOffset = lmtPriceOffset,
          adjustedStopPrice = adjustedStopPrice,
          adjustedStopLimitPrice = adjustedStopLimitPrice,
          adjustedTrailingAmount = adjustedTrailingAmount,
          adjustableTrailingUnit = adjustableTrailingUnit,
          softDollarTier = softDollarTier,
          cashQty = cashQty,
          dontUseAutoPriceForHedge = dontUseAutoPriceForHedge,
          isOmsContainer = isOmsContainer,
          discretionaryUpToLimitPrice = discretionaryUpToLimitPrice,
          usePriceMgmtAlgo = usePriceMgmtAlgo,
          duration = duration,
          postToAts = postToAts,
          autoCancelParent = autoCancelParent,
          minTradeQty = minTradeQty,
          minCompeteSize = minCompeteSize,
          competeAgainstBestOffset = competeAgainstBestOffset,
          midOffsetAtWhole = midOffsetAtWhole,
          midOffsetAtHalf = midOffsetAtHalf
        )

      OpenOrder(
        orderId,
        contract.copy(
          comboLegsDescrip = comboLegFields.comboLegsDescrip,
          comboLegs = comboLegFields.contractComboLegs,
          deltaNeutralContract = contractDeltaNeutral
        ),
        order,
        orderState
      )
    }

  def processCompletedOrder(using serverVersion: IBClient.ServerVersion): DecoderState[CompletedOrder] =
    val version = Int.MaxValue
    for
      contract <- readContract(version)
      // read order fields
      action <- read[Action]
      totalQuantity <- read[Decimal]
      orderType <- read[Order.Type]
      lmtPrice <- if version < 29 then read[Double] else readDoubleMax // check version
      auxPrice <- if version < 30 then read[Double] else readDoubleMax // check version
      tif <- read[TimeInForce]
      ocaGroup <- read[String]
      account <- read[String]
      openClose <- read[String]
      origin <- read[Int]
      orderRef <- read[String]
      permId <- if version >= 4 then read[Int] else readNothing(-1)
      outsideRth <- if version >= 4 then read[Boolean] else readNothing(false)
      hidden <- if version >= 4 then read[Int].map(_ == 1) else readNothing(false)
      discretionaryAmt <- if version >= 4 then read[Double] else readNothing(Double.MaxValue)
      goodAfterTime <- if version >= 5 then read[String] else readNothing("")
      faFields <- readFAParams(version)
      modelCode <- read[String]
      goodTillDate <- if version >= 8 then read[String] else readNothing("")
      rule80A <- if version >= 9 then read[Rule80A] else readNothing(Rule80A.None)
      percentOffset <- if version >= 9 then read[Double] else readNothing(Double.MaxValue)
      settlingFirm <- if version >= 9 then read[String] else readNothing("")
      shortSaleFields <- readShortSaleParams(version)
      auctionStrategy <- if version >= 9 then read[AuctionStrategy] else readNothing(AuctionStrategy.None)
      boxOrderFields <- readBoxOrdrParams(version)
      pegToStkOrVolOrderFields <- readPegToStkOrVolOrderParams(version)
      displaySize <- if version >= 9 then read[Int] else readNothing(Int.MaxValue)
      sweepToFill <- if version >= 9 then read[Boolean] else readNothing(false)
      allOrNone <- if version >= 9 then read[Boolean] else readNothing(false)
      minQty <- if version >= 9 then read[Int] else readNothing(Int.MaxValue)
      ocaType <- if version >= 9 then read[OcaType] else readNothing(OcaType.None)
      triggerMethod <- if version >= 10 then read[TriggerMethod] else readNothing(TriggerMethod.Default)
      volOrderFields <- readVolOrderParams(version, false)
      // trailParams <- readTrailParams(version)
      _ <- if version >= 13 then readDoubleMax else readNothing(Double.MaxValue)
      trailingPercent <- if version >= 30 then readDoubleMax else readNothing(Double.MaxValue)
      comboLegFields <- readComboLegs(version)
      smartComboRoutingFields <- readSmartComboRoutingParams(version)
      scaleFields <- readScaleOrderParams(version)
      hedgeFields <- readHedgeParams(version)
      clearingAccount <- if version >= 19 then read[String] else readNothing("")
      clearingIntent <- if version >= 19 then read[String] else readNothing("")
      notHeld <- if version >= 22 then read[Boolean] else readNothing(false)
      contractDeltaNeutral <- readDeltaNeutral(version)
      algoFields <- readAlgoParams(version)
      solicited <- if version >= 33 then read[Boolean] else readNothing(false)
      orderStatus <- read[Order.Status]
      randomizeSize <- if version >= 34 then read[Boolean] else readNothing(false)
      randomizePrice <- if version >= 34 then read[Boolean] else readNothing(false)
      pegToBenchFields <- readPegToBenchParams(orderType)
      conditionsField <- readConditions
      trailStopPrice <- readDoubleMax
      lmtPriceOffset <- readDoubleMax
      cashQty <- readDoubleMax
      dontUseAutoPriceForHedge <- read[Boolean]
      isOmsContainer <- if version >= 33 then read[Boolean] else readNothing(false)
      autoCancelDate <- read[String]
      filledQuantity <- read[Decimal]
      refFuturesConId <- read[Int]
      autoCancelParent <- if serverVersion >= MIN_VERSION then read[Boolean] else readNothing(false)
      shareholder <- read[String]
      imbalanceOnly <- read[Boolean]
      routeMarketableToBbo <- read[Boolean]
      parentPermId <- read[Long]
      completedTime <- read[String]
      completedStatus <- read[Order.Status]
      minTradeQty <- readIntMax
      minCompeteSize <- readIntMax
      competeAgainstBestOffset <- readDoubleMax
      midOffsetAtWhole <- readDoubleMax
      midOffsetAtHalf <- readDoubleMax
    yield
      val order = Order(
        action = action,
        totalQuantity = totalQuantity,
        orderType = orderType,
        lmtPrice = lmtPrice, // check version
        auxPrice = auxPrice, // check version
        tif = tif,
        ocaGroup = ocaGroup,
        account = account,
        openClose = openClose,
        origin = origin,
        orderRef = orderRef,
        permId = permId,
        outsideRth = outsideRth,
        hidden = hidden,
        discretionaryAmt = discretionaryAmt,
        goodAfterTime = goodAfterTime,
        faFields = faFields,
        modelCode = modelCode,
        goodTillDate = goodTillDate,
        rule80A = rule80A,
        percentOffset = percentOffset,
        settlingFirm = settlingFirm,
        shortSaleFields = shortSaleFields,
        auctionStrategy = auctionStrategy,
        boxOrderFields = boxOrderFields,
        pegToStkOrVolOrderFields = pegToStkOrVolOrderFields,
        displaySize = displaySize,
        sweepToFill = sweepToFill,
        allOrNone = allOrNone,
        minQty = minQty,
        ocaType = ocaType,
        triggerMethod = triggerMethod,
        volOrderFields = volOrderFields,
        // trailingParams = trailParams,
        orderComboLegs = comboLegFields.orderComboLegs,
        smartComboRoutingFields = smartComboRoutingFields,
        scaleFields = scaleFields,
        hedgeFields = hedgeFields,
        clearingAccount = clearingAccount,
        clearingIntent = clearingIntent,
        notHeld = notHeld,
        algoFields = algoFields,
        solicited = solicited,
        randomizeSize = randomizeSize,
        randomizePrice = randomizePrice,
        pegToBenchFields = pegToBenchFields,
        conditionFields = conditionsField,
        trailStopPrice = trailStopPrice,
        lmtPriceOffset = lmtPriceOffset,
        cashQty = cashQty,
        dontUseAutoPriceForHedge = dontUseAutoPriceForHedge,
        isOmsContainer = isOmsContainer,
        autoCancelDate = autoCancelDate,
        filledQuantity = filledQuantity,
        refFuturesConId = refFuturesConId,
        autoCancelParent = autoCancelParent,
        shareholder = shareholder,
        imbalanceOnly = imbalanceOnly,
        routeMarketableToBbo = imbalanceOnly,
        parentPermId = parentPermId,
        minTradeQty = minTradeQty,
        minCompeteSize = minCompeteSize,
        competeAgainstBestOffset = competeAgainstBestOffset,
        midOffsetAtWhole = midOffsetAtWhole,
        midOffsetAtHalf = midOffsetAtHalf
      )
      CompletedOrder(
        contract.copy(
          comboLegsDescrip = comboLegFields.comboLegsDescrip,
          comboLegs = comboLegFields.contractComboLegs,
          deltaNeutralContract = contractDeltaNeutral
        ),
        order,
        Order.State(
          status = orderStatus,
          completedTime = completedTime,
          completedStatus = completedStatus
        )
      )
}
