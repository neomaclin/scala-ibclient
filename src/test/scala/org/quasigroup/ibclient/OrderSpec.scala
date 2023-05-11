package org.quasigroup.ibclient

import org.quasigroup.ibclient.types.*

class OrderSpec:

  def AtAuction(action: Action, quantity: Decimal, price: Double): Order =
    Order(
      action = action,
      tif = TimeInForce.AUC,
      orderType = Order.Type.MTL,
      totalQuantity = quantity,
      lmtPrice = price
    )

  def Discretionary(action: Action, quantity: Decimal, price: Double, discretionaryAmt: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.LMT,
      totalQuantity = quantity,
      lmtPrice = price,
      discretionaryAmt = discretionaryAmt
    )

  def MarketOrder(action: Action, quantity: Decimal): Order =
    Order(
      action = action,
      orderType = Order.Type.MKT,
      totalQuantity = quantity
    )

  def MarketIfTouched(action: Action, quantity: Decimal, price: Double): Order = Order(
    action = action,
    orderType = Order.Type.MIT,
    totalQuantity = quantity,
    auxPrice = price
  )

  def MarketOnClose(action: Action, quantity: Decimal): Order = Order(
    action = action,
    orderType = Order.Type.MOC,
    totalQuantity = quantity
  )

  def MarketOnOpen(action: Action, quantity: Decimal): Order =
    Order(
      action = action,
      orderType = Order.Type.MKT,
      totalQuantity = quantity,
      tif = TimeInForce.OPG
    )

  def MidpointMatch(action: Action, quantity: Decimal): Order =
    Order(
      action = action,
      orderType = Order.Type.MKT,
      totalQuantity = quantity
    )

  // def Midprice(action: Action , quantity: Decimal, price: DoubleCap): Order =

  // 	Order(
  // 	action = action,
  // 	orderType = Order.Type.MIDPRICE,
  // 	totalQuantity = quantity,
  // 	lmtPrice = priceCap, // optional

  // 	)
  // }

  def PeggedToMarket(action: Action, quantity: Decimal, marketOffset: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.PEG_MKT,
      totalQuantity = Decimal.ONE_HUNDRED,
      auxPrice = marketOffset // Offset price
    )

  def PeggedToStock(
      action: Action,
      quantity: Decimal,
      delta: Double,
      stockReferencePrice: Double,
      startingPrice: Double
  ): Order =
    Order(
      action = action,
      orderType = Order.Type.PEG_STK,
      totalQuantity = quantity,
      boxOrderFields = Some(
        Order.BoxOrderParams(
          delta = delta,
          stockRefPrice = stockReferencePrice,
          startingPrice = startingPrice
        )
      )
    )

  def RelativePeggedToPrimary(action: Action, quantity: Decimal, priceCap: Double, offsetAmount: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.REL,
      totalQuantity = quantity,
      lmtPrice = priceCap,
      auxPrice = offsetAmount
    )

  def SweepToFill(action: Action, quantity: Decimal, price: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.LMT,
      totalQuantity = quantity,
      lmtPrice = price,
      sweepToFill = true
    )

  def AuctionLimit(action: Action, quantity: Decimal, price: Double, auctionStrategy: AuctionStrategy): Order =
    Order(
      action = action,
      orderType = Order.Type.LMT,
      totalQuantity = quantity,
      lmtPrice = price,
      auctionStrategy = auctionStrategy
    )

  def AuctionPeggedToStock(action: Action, quantity: Decimal, startingPrice: Double, delta: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.PEG_STK,
      totalQuantity = quantity,
      boxOrderFields = Some(
        Order.BoxOrderParams(
          delta = delta,
          startingPrice = startingPrice
        )
      )
    )

  def AuctionRelative(action: Action, quantity: Decimal, offset: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.REL,
      totalQuantity = quantity,
      auxPrice = offset
    )

  def Block(action: Action, quantity: Decimal, price: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.LMT,
      totalQuantity = quantity, // Large volumes!
      lmtPrice = price,
      blockOrder = true
    )

  def BoxTop(action: Action, quantity: Decimal): Order =
    Order(
      action = action,
      orderType = Order.Type.BOX_TOP,
      totalQuantity = quantity
    )

  def LimitOrder(action: Action, quantity: Decimal, limitPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.LMT,
      totalQuantity = quantity,
      lmtPrice = limitPrice,
      tif = TimeInForce.DAY
    )

  def LimitOrderWithManualOrderTime(
      action: Action,
      quantity: Decimal,
      limitPrice: Double,
      manualOrderTime: String
  ): Order =
    LimitOrder(action, quantity, limitPrice).copy(manualOrderTime = manualOrderTime)

  // Forex orders can be placed in denomination of second currency in pair using cashQty field
  // Requires TWS or IBG 963+
  // https://www.interactivebrokers.com/en/index.php?f=23876#963-02

  def LimitOrderWithCashQty(action: Action, limitPrice: Double, cashQty: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.LMT,
      lmtPrice = limitPrice,
      cashQty = cashQty
    )

  def LimitIfTouched(action: Action, quantity: Decimal, limitPrice: Double, triggerPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.LIT,
      totalQuantity = quantity,
      lmtPrice = limitPrice,
      auxPrice = triggerPrice
    )

  def LimitOnClose(action: Action, quantity: Decimal, limitPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.LOC,
      totalQuantity = quantity,
      lmtPrice = limitPrice
    )

  def LimitOnOpen(action: Action, quantity: Decimal, limitPrice: Double): Order =
    Order(
      action = action,
      tif = TimeInForce.OPG,
      orderType = Order.Type.LOC,
      totalQuantity = quantity,
      lmtPrice = limitPrice
    )

  def PassiveRelative(action: Action, quantity: Decimal, offset: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.PASSV_REL,
      totalQuantity = quantity,
      auxPrice = offset
    )

  def PeggedToMidpoint(action: Action, quantity: Decimal, offset: Double, limitPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.PEG_MID,
      totalQuantity = quantity,
      auxPrice = offset,
      lmtPrice = limitPrice
    )

  def BracketOrder(
      parentOrderId: Int,
      action: Action,
      quantity: Decimal,
      limitPrice: Double,
      takeProfitLimitPrice: Double,
      stopLossPrice: Double
  ): List[Order] = {
    // This will be our main or "parent" order
    val parent = Order(
      orderId = parentOrderId,
      action = action,
      orderType = Order.Type.LMT,
      totalQuantity = quantity,
      lmtPrice = limitPrice,
      // The parent and children orders will need this attribute set to false to prevent accidental executions.
      // The LAST CHILD will have it set to true.
      transmit = false
    )

    val takeProfit = Order(
      orderId = parent.orderId + 1,
      action = if action == Action.BUY then Action.SELL else Action.BUY,
      orderType = Order.Type.LMT,
      totalQuantity = quantity,
      lmtPrice = takeProfitLimitPrice,
      parentId = parentOrderId,
      transmit = false
    )

    val stopLoss = Order(
      orderId = parent.orderId + 2,
      action = if action == Action.BUY then Action.SELL else Action.BUY,
      orderType = Order.Type.STP,
      // Stop trigger price
      auxPrice = stopLossPrice,
      totalQuantity = quantity,
      parentId = parentOrderId,
      transmit = true
    )

    List(parent, takeProfit, stopLoss)
  }

  def MarketToLimit(action: Action, quantity: Decimal): Order =
    Order(
      action = action,
      orderType = Order.Type.MTL,
      totalQuantity = quantity
    )

  def MarketWithProtection(action: Action, quantity: Decimal): Order =
    Order(
      action = action,
      orderType = Order.Type.MKT_PRT,
      totalQuantity = quantity
    )

  def Stop(action: Action, quantity: Decimal, stopPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.STP,
      auxPrice = stopPrice,
      totalQuantity = quantity
    )

  def StopLimit(action: Action, quantity: Decimal, limitPrice: Double, stopPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.STP_LMT,
      lmtPrice = limitPrice,
      auxPrice = stopPrice,
      totalQuantity = quantity
    )

  def StopWithProtection(action: Action, quantity: Decimal, stopPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.STP_PRT,
      auxPrice = stopPrice,
      totalQuantity = quantity
    )

  def TrailingStop(action: Action, quantity: Decimal, trailingPercent: Double, trailStopPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.TRAIL,
      trailStopPrice = trailStopPrice,
      trailingPercent = trailingPercent,
      totalQuantity = quantity
    )

  def TrailingStopLimit(
      action: Action,
      quantity: Decimal,
      lmtPriceOffset: Double,
      trailingAmount: Double,
      trailStopPrice: Double
  ): Order =
    Order(
      action = action,
      orderType = Order.Type.TRAIL_LIMIT,
      lmtPriceOffset = lmtPriceOffset,
      auxPrice = trailingAmount,
      trailStopPrice = trailStopPrice,
      totalQuantity = quantity
    )

  def ComboLimitOrder(action: Action, quantity: Decimal, nonGuaranteed: Boolean, limitPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.LMT,
      lmtPrice = limitPrice,
      totalQuantity = quantity,
      smartComboRoutingFields = if nonGuaranteed then List(TagValue("NonGuaranteed", "1")) else Nil
    )

  def ComboMarketOrder(action: Action, quantity: Decimal, nonGuaranteed: Boolean): Order =
    Order(
      action = action,
      orderType = Order.Type.MKT,
      totalQuantity = quantity,
      smartComboRoutingFields = if nonGuaranteed then List(TagValue("NonGuaranteed", "1")) else Nil
    )

  def LimitOrderForComboWithLegPrices(
      action: Action,
      quantity: Decimal,
      nonGuaranteed: Boolean,
      legPrices: List[Double]
  ): Order =
    Order(
      action = action,
      orderType = Order.Type.LMT,
      totalQuantity = quantity,
      smartComboRoutingFields = if nonGuaranteed then List(TagValue("NonGuaranteed", "1")) else Nil,
      orderComboLegs = legPrices.map(Order.ComboLeg(_))
    )

  def RelativeLimitCombo(action: Action, quantity: Decimal, nonGuaranteed: Boolean, limitPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.REL_PLUS_MKT,
      totalQuantity = quantity,
      lmtPrice = limitPrice,
      smartComboRoutingFields = if nonGuaranteed then List(TagValue("NonGuaranteed", "1")) else Nil
    )

  def RelativeMarketCombo(action: Action, quantity: Decimal, nonGuaranteed: Boolean): Order =
    Order(
      action = action,
      orderType = Order.Type.REL_PLUS_MKT,
      totalQuantity = quantity,
      smartComboRoutingFields = if nonGuaranteed then List(TagValue("NonGuaranteed", "1")) else Nil
    )

  def OneCancelsAll(ocaGroup: String, ocaOrders: List[Order], ocaType: OcaType): List[Order] =
    ocaOrders.map(_.copy(ocaGroup = ocaGroup, ocaType = ocaType))

  def Volatility(
      action: Action,
      quantity: Decimal,
      volatilityPercent: Double,
      volatilityType: VolatilityType
  ): Order =
    Order(
      action = action,
      orderType = Order.Type.VOL,
      volOrderFields = Some(
        Order.VolOrderParams(
          volatility = volatilityPercent, // Expressed in percentage (40%)
          volatilityType = volatilityType // 1=daily, 2=annual
        )
      ),
      totalQuantity = quantity
    )

  def MarketFHedge(parentOrderId: Int, action: Action): Order =
    // FX Hedge orders can only have a quantity of 0
    MarketOrder(action, Decimal.ZERO).copy(
      parentId = parentOrderId,
      hedgeFields = Some(Order.HedgeParams(HedgeType.Fx))
    )

  def PeggedToBenchmark(
      action: Action,
      quantity: Decimal,
      startingPrice: Double,
      peggedChangeAmountDecrease: Boolean,
      peggedChangeAmount: Double,
      referenceChangeAmount: Double,
      referenceConId: Int,
      referenceExchange: String,
      stockReferencePrice: Double,
      referenceContractLowerRange: Double,
      referenceContractUpperRange: Double
  ): Order =
    Order(
      orderType = Order.Type.PEG_BENCH,
      // BUY or SELL
      action = action,
      totalQuantity = quantity,
      // Beginning with price...

      pegToBenchFields = Some(
        Order.PegToBenchParams(
          isPeggedChangeAmountDecrease = peggedChangeAmountDecrease,
          // by... (and likewise for price moving in opposite direction)
          peggedChangeAmount = peggedChangeAmount,
          // whenever there is a price change of...
          referenceChangeAmount = referenceChangeAmount,
          // in the reference contract...
          referenceContractId = referenceConId,
          // being traded at...
          referenceExchangeId = referenceExchange
        )
      ),
      boxOrderFields = Some(
        Order.BoxOrderParams(
          // increase/decrease price...
          startingPrice = startingPrice,
          // starting reference price is...
          stockRefPrice = stockReferencePrice
        )
      ),
      pegToStkOrVolOrderFields = Some(
        Order.PegToStkOrVolOrderParams(
          // Keep order active as long as reference contract trades between...
          stockRangeLower = referenceContractLowerRange,
          // and...
          stockRangeUpper = referenceContractUpperRange
        )
      )
    )

  def AttachAdjustableToStop(
      parent: Order,
      attachedOrderStopPrice: Double,
      triggerPrice: Double,
      adjustStopPrice: Double
  ): Order =
    Order(
      // Attached order is a conventional STP order in opposite direction
      action = if parent.action == Action.BUY then Action.SELL else Action.BUY,
      totalQuantity = parent.totalQuantity,
      auxPrice = attachedOrderStopPrice,
      parentId = parent.orderId,
      // When trigger price is penetrated
      triggerPrice = triggerPrice,
      // The parent order will be turned into a STP order
      adjustedOrderType = Order.Type.STP,
      // With the given STP price
      adjustedStopPrice = adjustStopPrice
    )

  def AttachAdjustableToStopLimit(
      parent: Order,
      attachedOrderStopPrice: Double,
      triggerPrice: Double,
      adjustStopPrice: Double,
      adjustedStopLimitPrice: Double
  ): Order =
    Order(
      // Attached order is a conventional STP order
      action = if parent.action == Action.BUY then Action.SELL else Action.BUY,
      totalQuantity = parent.totalQuantity,
      auxPrice = attachedOrderStopPrice,
      parentId = parent.orderId,
      // When trigger price is penetrated
      triggerPrice = triggerPrice,
      // The parent order will be turned into a STLMT order
      adjustedOrderType = Order.Type.STP_LMT,
      // With the given stop price
      adjustedStopPrice = adjustStopPrice,
      // And the given limit price
      adjustedStopLimitPrice = adjustedStopLimitPrice
    )

  def AttachAdjustableToTrail(
      parent: Order,
      attachedOrderStopPrice: Double,
      triggerPrice: Double,
      adjustStopPrice: Double,
      adjustedTrailAmount: Double,
      trailUnit: Int
  ): Order =
    Order(
      // Attached order is a conventional STP order
      action = if parent.action == Action.BUY then Action.SELL else Action.BUY,
      totalQuantity = parent.totalQuantity,
      auxPrice = attachedOrderStopPrice,
      parentId = parent.orderId,
      // When trigger price is penetrated
      triggerPrice = triggerPrice,
      // The parent order will be turned into a TRAIL order
      adjustedOrderType = Order.Type.TRAIL,
      // With a stop price of...
      adjustedStopPrice = adjustStopPrice,
      // trailing by and amount (0) or a percent (100)...
      adjustableTrailingUnit = trailUnit,
      // of...
      adjustedTrailingAmount = adjustedTrailAmount
    )

  def priceCondition(
      conId: Int,
      exchange: String,
      price: Double,
      isMore: Boolean,
      isConjunction: Boolean
  ): PriceCondition =
    // Conditions have to be created via the OrderCondition.Create
    PriceCondition(
      // When this contract...
      conId = conId,
      // traded on this exchange
      exchange = exchange,
      // has a price above/below
      isMore = isMore,
      // this quantity
      price = price,
      // AND | OR next condition (will be ignored if no more conditions are added)
      isConjunctionConnection = isConjunction
    )

  def executionCondition(
      symbol: String,
      secType: SecType,
      exchange: String,
      isConjunction: Boolean
  ): ExecutionCondition =
    ExecutionCondition(
      // When an execution on symbol
      symbol = symbol,
      // at exchange
      exchange = exchange,
      // for this secType
      secType = secType,
      // AND | OR next condition (will be ignored if no more conditions are added)
      isConjunctionConnection = isConjunction
    )

  def marginCondition(percent: Int, isMore: Boolean, isConjunction: Boolean): MarginCondition =
    MarginCondition(
      // If margin is above/below
      isMore = isMore,
      // given percent
      percent = percent,
      // AND | OR next condition (will be ignored if no more conditions are added)
      isConjunctionConnection = isConjunction
    )

  def percentageChangeCondition(
      pctChange: Double,
      conId: Int,
      exchange: String,
      isMore: Boolean,
      isConjunction: Boolean
  ): PercentChangeCondition =
    PercentChangeCondition(
      // If there is a price percent change measured against last close price above or below...
      // When this contract...
      conId = conId,
      // traded on this exchange
      exchange = exchange,
      // has a price above/below
      isMore = isMore,
      // this amount...
      changePercent = pctChange,
      // AND | OR next condition (will be ignored if no more conditions are added)
      isConjunctionConnection = isConjunction
    )

  def timeCondition(time: String, isMore: Boolean, isConjunction: Boolean): TimeCondition =
    TimeCondition(
      // Before or after...
      isMore = isMore,
      // this time...
      time = time,
      // AND | OR next condition (will be ignored if no more conditions are added)
      isConjunctionConnection = isConjunction
    )

  def volumeCondition(
      conId: Int,
      exchange: String,
      volume: Int,
      isMore: Boolean,
      isConjunction: Boolean
  ): VolumeCondition =
    VolumeCondition(
      // When this contract...
      conId = conId,
      // traded on this exchange
      exchange = exchange,
      // has a price above/below
      isMore = isMore,
      // than this...
      volume = volume,
      // AND | OR next condition (will be ignored if no more conditions are added)
      isConjunctionConnection = isConjunction
    )

  def WhatIfLimitOrder(action: Action, quantity: Decimal, limitPrice: Double): Order =
    LimitOrder(action, quantity, limitPrice).copy(whatIf = true)

  def LimitIBKRATS(action: Action, quantity: Decimal, limitPrice: Double): Order =
    Order(
      action = action,
      orderType = Order.Type.LMT,
      lmtPrice = limitPrice,
      totalQuantity = quantity,
      notHeld = true
    )

  def PegBestUpToMidOrder(
      action: Action,
      quantity: Decimal,
      limitPrice: Double,
      minTradeQty: Int,
      minCompeteSize: Int,
      midOffsetAtWhole: Double,
      midOffsetAtHalf: Double
  ): Order =
    Order(
      action = action,
      orderType = Order.Type.PEG_BEST,
      lmtPrice = limitPrice,
      totalQuantity = quantity,
      notHeld = true,
      minTradeQty = minTradeQty,
      minCompeteSize = minCompeteSize,
      competeAgainstBestOffset = Order.COMPETE_AGAINST_BEST_OFFSET_UP_TO_MID,
      midOffsetAtWhole = midOffsetAtWhole,
      midOffsetAtHalf = midOffsetAtHalf
    )

  def PegBestOrder(
      action: Action,
      quantity: Decimal,
      limitPrice: Double,
      minTradeQty: Int,
      minCompeteSize: Int,
      competeAgainstBestOffset: Double
  ): Order =
    Order(
      action = action,
      orderType = Order.Type.PEG_BEST,
      lmtPrice = limitPrice,
      totalQuantity = quantity,
      notHeld = true,
      minTradeQty = minTradeQty,
      minCompeteSize = minCompeteSize,
      competeAgainstBestOffset = competeAgainstBestOffset
    )

  def PegMidOrder(
      action: Action,
      quantity: Decimal,
      limitPrice: Double,
      minTradeQty: Int,
      midOffsetAtWhole: Double,
      midOffsetAtHalf: Double
  ): Order =
    Order(
      action = action,
      orderType = Order.Type.PEG_MID,
      lmtPrice = limitPrice,
      totalQuantity = quantity,
      notHeld = true,
      minTradeQty = minTradeQty,
      midOffsetAtWhole = midOffsetAtWhole,
      midOffsetAtHalf = midOffsetAtHalf
    )
