package org.quasigroup.ibclient.response

import org.quasigroup.ibclient.types.*

enum ResponseMsg:
  case TickPrice(
      tickerId: Int,
      priceTickType: TickType,
      price: Double,
      attribs: TickAttrib,
      tickSize: Option[TickSize]
  ) extends ResponseMsg

  case TickSize(tickerId: Int, sizeTickType: TickType, size: Decimal)
      extends ResponseMsg

  case TickOptionComputation(
      tickerId: Int,
      tickType: TickType,
      impliedVol: Double,
      delta: Double,
      optPrice: Double,
      pvDividend: Double,
      gamma: Double,
      vega: Double,
      theta: Double,
      undPrice: Double
  ) extends ResponseMsg

  case TickGeneric(tickerId: Int, tickType: TickType, value: Double)
      extends ResponseMsg

  case TickString(tickerId: Int, tickType: TickType, value: String)
      extends ResponseMsg

  case TickEFP(
      tickerId: Int,
      tickType: TickType,
      basisPoInts: Double,
      formattedBasisPoInts: String,
      impliedFuture: Double,
      holdDays: Int,
      futureExpiry: String,
      dividendImpact: Double,
      dividendsToExpiry: Double
  ) extends ResponseMsg

  case OrderStatus(
      orderId: Int,
      status: String,
      filled: Int,
      remaining: Int,
      avgFillPrice: Double,
      permId: Int,
      parentId: Int,
      lastFillPrice: Double,
      clientId: Int,
      whyHeld: String,
      mktCapPrice: Double
  ) extends ResponseMsg

  case OpenOrder(
      orderId: Int,
      contract: Contract,
      order: Order,
      orderState: Order.State
  ) extends ResponseMsg

  case OpenOrderEnd extends ResponseMsg

  case UpdateAccountValue(
      key: String,
      value: String,
      currency: String,
      accountName: String
  ) extends ResponseMsg

  case UpdatePortfolio(
      contract: Contract,
      position: Decimal,
      marketPrice: Double,
      marketValue: Double,
      averageCost: Double,
      unrealizedPNL: Double,
      realizedPNL: Double,
      accountName: String
  ) extends ResponseMsg

  case UpdateAccountTime(timeStamp: String) extends ResponseMsg

  case AccountDownloadEnd(accountName: String) extends ResponseMsg

  case NextValidId(orderId: Int) extends ResponseMsg

  case ContractDetails(reqId: Int, contractDetails: ContractDetails)
      extends ResponseMsg

  case BondContractDetails(reqId: Int, contractDetails: ContractDetails)
      extends ResponseMsg

  case ContractDetailsEnd(reqId: Int) extends ResponseMsg

  case ExecDetails(reqId: Int, contract: Contract, execution: Execution)
      extends ResponseMsg

  case ExecDetailsEnd(reqId: Int) extends ResponseMsg

  case UpdateMktDepth(
      tickerId: Int,
      position: Int,
      operation: Int,
      side: Int,
      price: Double,
      size: Decimal
  ) extends ResponseMsg

  case UpdateMktDepthL2(
      tickerId: Int,
      position: Int,
      marketMaker: String,
      operation: Int,
      side: Int,
      price: Double,
      size: Decimal,
      isSmartDepth: Boolean
  ) extends ResponseMsg

  case UpdateNewsBulletin(
      msgId: Int,
      msgType: NewsType,
      message: String,
      origExchange: String
  ) extends ResponseMsg

  case ManagedAccounts(accountsList: String) extends ResponseMsg

  case ReceiveFA(faDataType: Int, xml: String) extends ResponseMsg

  case HistoricalData(
      histories: List[HistoricalDataUpdate],
      startDate: String,
      endDate: String
  ) extends ResponseMsg

  case ScannerParameters(xml: String) extends ResponseMsg

  case ScannerData(
      reqId: Int,
      rank: Int,
      contractDetails: ContractDetails,
      distance: String,
      benchmark: String,
      projection: String,
      legsStr: String
  ) extends ResponseMsg

  case ScannerDataEnd(reqId: Int) extends ResponseMsg

  case RealtimeBar(
      reqId: Int,
      time: Long,
      open: Double,
      high: Double,
      low: Double,
      close: Double,
      volume: Decimal,
      wap: Decimal,
      count: Int
  ) extends ResponseMsg

  case CurrentTime(time: Long) extends ResponseMsg

  case FundamentalData(reqId: Int, data: String) extends ResponseMsg

  case DeltaNeutralValidation(reqId: Int, underComp: DeltaNeutralContract)
      extends ResponseMsg

  case TickSnapshotEnd(reqId: Int) extends ResponseMsg

  case MarketDataType(reqId: Int, marketDataType: MktDataType)
      extends ResponseMsg

  case CommissionReportMsg(commissionReport: CommissionReport)
      extends ResponseMsg

  case PositionMsg(
      account: String,
      contract: Contract,
      pos: Decimal,
      avgCost: Double
  ) extends ResponseMsg

  case PositionEnd extends ResponseMsg

  case AccountSummary(
      reqId: Int,
      account: String,
      tag: String,
      value: String,
      currency: String
  ) extends ResponseMsg

  case AccountSummaryEnd(reqId: Int) extends ResponseMsg

  case VerifyMessageAPI(apiData: String) extends ResponseMsg

  case VerifyCompleted(isSuccessful: Boolean, errorText: String)
      extends ResponseMsg

  case VerifyAndAuthMessageAPI(apiData: String, xyzChallange: String)
      extends ResponseMsg

  case VerifyAndAuthCompleted(isSuccessful: Boolean, errorText: String)
      extends ResponseMsg

  case DisplayGroupList(reqId: Int, groups: String) extends ResponseMsg

  case DisplayGroupUpdated(reqId: Int, contractInfo: String) extends ResponseMsg

  case ErrorDetail(
      id: Int,
      errorCode: Int,
      errorMsg: String,
      advancedOrderRejectJson: String
  ) extends ResponseMsg

  case ConnectionClosed extends ResponseMsg

  case PositionMulti(
      reqId: Int,
      account: String,
      modelCode: String,
      contract: Contract,
      pos: Decimal,
      avgCost: Double
  ) extends ResponseMsg

  case PositionMultiEnd(reqId: Int) extends ResponseMsg

  case AccountUpdateMulti(
      reqId: Int,
      account: String,
      modelCode: String,
      key: String,
      value: String,
      currency: String
  ) extends ResponseMsg

  case AccountUpdateMultiEnd(reqId: Int) extends ResponseMsg

  case SecurityDefinitionOptionalParameter(
      reqId: Int,
      exchange: String,
      underlyingConId: Int,
      tradingClass: String,
      multiplier: String,
      expirations: Set[String],
      strikes: Set[Double]
  ) extends ResponseMsg

  case SecurityDefinitionOptionalParameterEnd(reqId: Int) extends ResponseMsg

  case SoftDollarTiers(reqId: Int, tiers: List[SoftDollarTier])
      extends ResponseMsg

  case FamilyCodes(familyCodes: List[FamilyCode]) extends ResponseMsg

  case SymbolSamples(
      reqId: Int,
      contractDescriptions: List[ContractDescription]
  ) extends ResponseMsg

  case MktDepthExchanges(
      depthMktDataDescriptions: List[DepthMktDataDescription]
  ) extends ResponseMsg

  case TickNews(
      tickerId: Int,
      timeStamp: Long,
      providerCode: String,
      articleId: String,
      headline: String,
      extraData: String
  ) extends ResponseMsg

  case SmartComponents(reqId: Int, theMap: Map[Int, (String, Char)])
      extends ResponseMsg

  case TickReqParams(
      tickerId: Int,
      minTick: Double,
      bboExchange: String,
      snapshotPermissions: Int
  ) extends ResponseMsg

  case NewsProviders(newsProviders: List[NewsProvider]) extends ResponseMsg

  case NewsArticle(requestId: Int, articleType: Int, articleText: String)
      extends ResponseMsg

  case HistoricalNews(
      requestId: Int,
      time: String,
      providerCode: String,
      articleId: String,
      headline: String
  ) extends ResponseMsg

  case HistoricalNewsEnd(requestId: Int, hasMore: Boolean) extends ResponseMsg

  case HeadTimestamp(reqId: Int, headTimestamp: String) extends ResponseMsg

  case HistogramData(reqId: Int, items: List[HistogramEntry])
      extends ResponseMsg

  case HistoricalDataUpdate(reqId: Int, bar: Bar) extends ResponseMsg

  case RerouteMktDataReq(reqId: Int, conId: Int, exchange: String)
      extends ResponseMsg

  case RerouteMktDepthReq(reqId: Int, conId: Int, exchange: String)
      extends ResponseMsg

  case MarketRule(marketRuleId: Int, priceIncrements: List[PriceIncrement])
      extends ResponseMsg

  case PnL(
      reqId: Int,
      dailyPnL: Double,
      unrealizedPnL: Double,
      realizedPnL: Double
  ) extends ResponseMsg

  case PnLSingle(
      reqId: Int,
      pos: Decimal,
      dailyPnL: Double,
      unrealizedPnL: Double,
      realizedPnL: Double,
      value: Double
  ) extends ResponseMsg

  case HistoricalTicks(reqId: Int, ticks: List[HistoricalTick], done: Boolean)
      extends ResponseMsg

  case HistoricalTicksBidAsk(
      reqId: Int,
      ticks: List[HistoricalTickBidAsk],
      done: Boolean
  ) extends ResponseMsg

  case HistoricalTicksLast(
      reqId: Int,
      ticks: List[HistoricalTickLast],
      done: Boolean
  ) extends ResponseMsg

  case TickByTickAllLast(
      reqId: Int,
      tickType: Int,
      time: Long,
      price: Double,
      size: Decimal,
      tickAttribLast: TickAttribLast,
      exchange: String,
      specialConditions: String
  ) extends ResponseMsg

  case TickByTickBidAsk(
      reqId: Int,
      time: Long,
      bidPrice: Double,
      askPrice: Double,
      bidSize: Decimal,
      askSize: Decimal,
      tickAttribBidAsk: TickAttribBidAsk
  ) extends ResponseMsg

  case TickByTickMidPoint(reqId: Int, time: Long, midPoint: Double)
      extends ResponseMsg

  case OrderBound(orderId: Long, apiClientId: Int, apiOrderId: Int)
      extends ResponseMsg

  case CompletedOrder(contract: Contract, order: Order, orderState: Order.State)
      extends ResponseMsg

  case CompletedOrdersEnd extends ResponseMsg

  case ReplaceFAEnd(reqId: Int, text: String) extends ResponseMsg

  case WshMetaDataMsg(reqId: Int, dataJson: String) extends ResponseMsg

  case WshEventDataMsg(reqId: Int, dataJson: String) extends ResponseMsg

  case HistoricalSchedule(
      reqId: Int,
      startDateTime: String,
      endDateTime: String,
      timeZone: String,
      sessions: List[HistoricalSession]
  ) extends ResponseMsg

  case UserInfo(reqId: Int, whiteBrandingId: String) extends ResponseMsg

  case SecurityDefinitionOptionalParameterEndMsg(reqId: Int) extends ResponseMsg
