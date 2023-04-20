package org.quasigroup.ibclient.request

import MsgId.*
import org.quasigroup.ibclient.types.*

enum RequestMsg:
  case StartAPI(
      msgId: Int = START_API,
      version: Int = 2,
      clientId: Int,
      optionalCapabilities: String = ""
  ) extends RequestMsg

  case ReqFamilyCodes(msgId: Int = REQ_FAMILY_CODES) extends RequestMsg

  case ReqMatchingSymbols(
      msgId: Int = REQ_MATCHING_SYMBOLS,
      reqId: Int,
      pattern: String
  ) extends RequestMsg

  case ReqMktDepthExchanges(msgId: Int = REQ_MKT_DEPTH_EXCHANGES)
      extends RequestMsg

  case ReqreqSmartComponents(
      msgId: Int = REQ_SMART_COMPONENTS,
      reqId: Int,
      bboExchange: String
  ) extends RequestMsg

  case ReqNewsArticle(
      msgId: Int = REQ_NEWS_ARTICLE,
      requestId: Int,
      providerCode: String,
      articleId: String,
      newsArticleOptions: List[TagValue]
  ) extends RequestMsg

  case ReqNewsProviders(msgId: Int = REQ_NEWS_PROVIDERS) extends RequestMsg

  case ReqHistoricalNews(
      msgId: Int = REQ_HISTORICAL_NEWS,
      requestId: Int,
      conId: Int,
      providerCodes: String,
      startDateTime: String,
      endDateTime: String,
      totalResults: Int,
      historicalNewsOptions: List[TagValue]
  ) extends RequestMsg

  case ReqScannerSubscription(
      msgId: Int = REQ_SCANNER_SUBSCRIPTION,
      version: Int = 4,
      tickerId: Int,
      subscription: ScannerSubscription,
      scannerSubscriptionOptions: List[TagValue]
  ) extends RequestMsg

  case CancelScannerSubscription(
      msgId: Int = CANCEL_SCANNER_SUBSCRIPTION,
      version: Int = 1,
      tickerId: Int
  ) extends RequestMsg

  case ReqMktData(
      msgId: Int = REQ_MKT_DATA,
      version: Int = 11,
      tickerId: Int,
      contract: Contract,
      genericTickList: String,
      snapshot: Boolean,
      regulatorySnapshot: Boolean,
      mktDataOptions: List[TagValue]
  ) extends RequestMsg

  case CancelMktData(
      msgId: Int = CANCEL_MKT_DATA,
      version: Int = 1,
      tickerId: Int
  ) extends RequestMsg

  case ReqHistoricalData(
      msgId: Int = REQ_HISTORICAL_DATA,
      version: Int = 6,
      tickerId: Int,
      contract: Contract,
      endDateTime: String,
      durationStr: String,
      barSizeSetting: String,
      whatToShow: String,
      useRTH: Int,
      formatDate: Int,
      keepUpToDate: Boolean,
      chartOptions: List[TagValue]
  ) extends RequestMsg

  case CancelHistoricalData(
      msgId: Int = CANCEL_HISTORICAL_DATA,
      version: Int = 1,
      tickerId: Int
  ) extends RequestMsg

  case ReqRealTimeBars(
      msgId: Int = REQ_REAL_TIME_BARS,
      version: Int = 3,
      tickerId: Int,
      contract: Contract,
      barSize: Int,
      whatToShow: String,
      useRTH: Boolean,
      realTimeBarsOptions: List[TagValue]
  ) extends RequestMsg

  case CancelRealTimeBars(
      msgId: Int = CANCEL_REAL_TIME_BARS,
      version: Int = 1,
      tickerId: Int
  ) extends RequestMsg

  case ReqScannerParameters(
      msgId: Int = REQ_SCANNER_PARAMETERS,
      version: Int = 1
  ) extends RequestMsg

  case ReqContractDetails(
      msgId: Int = REQ_CONTRACT_DATA,
      version: Int = 8,
      reqId: Int,
      contract: Contract
  ) extends RequestMsg

  case ReqMktDepth(
      msgId: Int = REQ_MKT_DEPTH,
      version: Int = 5,
      tickerId: Int,
      contract: Contract,
      numRows: Int,
      isSmartDepth: Boolean,
      mktDepthOptions: List[TagValue]
  ) extends RequestMsg

  case CancelMktDepth(
      msgId: Int = CANCEL_MKT_DEPTH,
      version: Int = 1,
      tickerId: Int
  ) extends RequestMsg

  case ExerciseOptions(
      msgId: Int = EXERCISE_OPTIONS,
      version: Int = 2,
      tickerId: Int,
      contract: Contract,
      exerciseAction: Int,
      exerciseQuantity: Int,
      account: String,
      `override`: Int
  ) extends RequestMsg

  case PlaceOrder(
      msgId: Int = PLACE_ORDER,
      id: Int,
      contract: Contract,
      order: Order
  ) extends RequestMsg

  case CancelOrder(msgId: Int = CANCEL_ORDER, version: Int = 1, id: Int)
      extends RequestMsg

  case ReqAccountUpdates(
      msgId: Int = REQ_ACCOUNT_DATA,
      version: Int = 2,
      subscribe: Boolean,
      acctCode: String
  ) extends RequestMsg

  case ReqAccountUpdatesMulit(
      msgId: Int = REQ_ACCOUNT_UPDATES_MULTI,
      version: Int = 1,
      reqId: Int,
      account: String,
      modelCode: String,
      ledgerAndNLV: Boolean
  ) extends RequestMsg

  case CancelAccountUpdatesMulit(
      msgId: Int = CANCEL_ACCOUNT_UPDATES_MULTI,
      version: Int = 1,
      reqId: Int
  ) extends RequestMsg

  case ReqSecDefOptParams(
      msgId: Int = REQ_SEC_DEF_OPT_PARAMS,
      reqId: Int,
      underlyingSymbol: String,
      futFopExchange: String,
      underlyingSecType: String,
      underlyingConId: Int
  ) extends RequestMsg

  case ReqSoftDollarTiers(
      msgId: Int = REQ_SOFT_DOLLAR_TIERS,
      reqId: Int
  ) extends RequestMsg

  case ReqExecutions(
      msgId: Int = REQ_EXECUTIONS,
      version: Int = 3,
      reqId: Int,
      filter: ExecutionFilter
  ) extends RequestMsg

  case ReqOpenOrders(
      msgId: Int = REQ_OPEN_ORDERS,
      version: Int = 1
  ) extends RequestMsg

  case ReqIds(msgId: Int = REQ_IDS, version: Int = 1, numIds: Int)
      extends RequestMsg

  case ReqNewsBulletins(
      msgId: Int = REQ_NEWS_BULLETINS,
      version: Int = 1,
      allMsgs: Boolean
  ) extends RequestMsg

  case CancelNewsBulletins(
      msgId: Int = CANCEL_NEWS_BULLETINS,
      version: Int = 1
  ) extends RequestMsg

  case SetServerLogLevel(
      msgId: Int = SET_SERVER_LOGLEVEL,
      version: Int = 1,
      loglevel: Int
  ) extends RequestMsg

  case ReqAutoOpenOrders(
      msgId: Int = REQ_AUTO_OPEN_ORDERS,
      version: Int = 1,
      bAutoBind: Boolean
  ) extends RequestMsg

  case ReqAllOpenOrders(msgId: Int = REQ_ALL_OPEN_ORDERS, version: Int = 1)
      extends RequestMsg

  case ReqManagedAccts(msgId: Int = REQ_MANAGED_ACCTS, version: Int = 1)
      extends RequestMsg

  case RequestFA(msgId: Int = REQ_FA, version: Int = 1, faDataType: Int)
      extends RequestMsg

  case ReplaceFA(
      msgId: Int = REPLACE_FA,
      version: Int = 1,
      faDataType: Int,
      xml: String
  ) extends RequestMsg

  case ReqCurrentTime(msgId: Int = REQ_CURRENT_TIME, version: Int = 1)
      extends RequestMsg

  case ReqFundamentalData(
      msgId: Int = REQ_FUNDAMENTAL_DATA,
      version: Int = 1,
      reqId: Int,
      contract: Contract,
      reportType: String,
      fundamentalDataOptions: List[TagValue]
  ) extends RequestMsg

  case CancelFundamentalData(
      msgId: Int = CANCEL_FUNDAMENTAL_DATA,
      version: Int = 1,
      reqId: Int
  ) extends RequestMsg

  case CalculateImpliedVolatility(
      msgId: Int = REQ_CALC_IMPLIED_VOLAT,
      version: Int = 2,
      reqId: Int,
      contract: Contract,
      optionPrice: Double,
      underPrice: Double,
      impliedVolatilityOptions: List[TagValue]
  ) extends RequestMsg

  case CancelCalculateImpliedVolatility(
      msgId: Int = CANCEL_CALC_IMPLIED_VOLAT,
      version: Int = 1,
      reqId: Int
  ) extends RequestMsg

  case CalculateOptionPrice(
      msgId: Int = REQ_CALC_OPTION_PRICE,
      version: Int = 2,
      reqId: Int,
      contract: Contract,
      volatility: Double,
      underPrice: Double,
      optionPriceOptions: List[TagValue]
  ) extends RequestMsg

  case CancelCalculateOptionPrice(
      msgId: Int = CANCEL_CALC_OPTION_PRICE,
      version: Int = 1,
      reqId: Int
  ) extends RequestMsg

  case ReqGlobalCancel(msgId: Int = REQ_GLOBAL_CANCEL, version: Int = 1)
      extends RequestMsg

  case ReqMarketDataType(
      msgId: Int = REQ_MARKET_DATA_TYPE,
      version: Int = 1,
      marketDataType: Int
  ) extends RequestMsg

  case ReqPositions(
      msgId: Int = REQ_POSITIONS,
      version: Int = 1
  ) extends RequestMsg

  case ReqPositionsMulti(
      msgId: Int = REQ_POSITIONS_MULTI,
      version: Int = 1,
      reqId: Int,
      account: String,
      modelCode: String
  ) extends RequestMsg

  case CancelPositions(
      msgId: Int = CANCEL_POSITIONS,
      version: Int = 1
  ) extends RequestMsg

  case CancelPositionsMulti(
      msgId: Int = CANCEL_POSITIONS_MULTI,
      version: Int = 1,
      reqId: Int
  ) extends RequestMsg

  case ReqAccountSummary(
      msgId: Int = REQ_ACCOUNT_SUMMARY,
      version: Int = 1,
      reqId: Int,
      group: String,
      tags: String
  ) extends RequestMsg

  case CancelAccountSummary(
      msgId: Int = CANCEL_ACCOUNT_SUMMARY,
      version: Int = 1,
      reqId: Int
  ) extends RequestMsg

  case VerifyRequest(
      msgId: Int = VERIFY_REQUEST,
      version: Int = 1,
      apiName: String,
      apiVersion: String
  ) extends RequestMsg

  case VerifyMessage(
      msgId: Int = VERIFY_MESSAGE,
      version: Int = 1,
      apiData: String
  ) extends RequestMsg

  case VerifyAndAuthRequest(
      msgId: Int = VERIFY_AND_AUTH_REQUEST,
      version: Int = 1,
      apiName: String,
      apiVersion: String,
      opaqueIsvKey: String
  ) extends RequestMsg

  case VerifyAndAuthMessage(
      msgId: Int = VERIFY_AND_AUTH_MESSAGE,
      version: Int = 1,
      apiData: String,
      xyzResponse: String
  ) extends RequestMsg

  case QueryDisplayGroups(
      msgId: Int = QUERY_DISPLAY_GROUPS,
      version: Int = 1,
      reqId: Int
  ) extends RequestMsg

  case SubscribeToGroupEvents(
      msgId: Int = SUBSCRIBE_TO_GROUP_EVENTS,
      version: Int = 1,
      reqId: Int,
      groupId: Int
  ) extends RequestMsg

  case UnsubscribeFromGroupEvents(
      msgId: Int = UNSUBSCRIBE_FROM_GROUP_EVENTS,
      version: Int = 1,
      reqId: Int
  ) extends RequestMsg

  case UpdateDisplayGroup(
      msgId: Int = UPDATE_DISPLAY_GROUP,
      version: Int = 1,
      reqId: Int,
      contractInfo: String
  ) extends RequestMsg

  case ReqHistogramData(
      msgId: Int = REQ_HISTOGRAM_DATA,
      tickerId: Int,
      contract: Contract,
      useRTH: Boolean,
      timePeriod: String
  ) extends RequestMsg

  case CancelHistogramData(msgId: Int = CANCEL_HISTOGRAM_DATA, tickerId: Int)
      extends RequestMsg

  case ReqMarketRule(msgId: Int = REQ_MARKET_RULE, marketRuleId: Int)
      extends RequestMsg

  case ReqPnL(
      msgId: Int = REQ_PNL,
      reqId: Int,
      account: String,
      modelCode: String
  ) extends RequestMsg

  case CancelPnL(msgId: Int = CANCEL_PNL, reqId: Int) extends RequestMsg

  case ReqPnLSingle(
      msgId: Int = REQ_PNL_SINGLE,
      reqId: Int,
      account: String,
      modelCode: String,
      conId: Int
  ) extends RequestMsg

  case CancelPnLSingle(msgId: Int = CANCEL_PNL_SINGLE, reqId: Int)
      extends RequestMsg

  case ReqHistoricalTicks(
      msgId: Int = REQ_HISTORICAL_TICKS,
      reqId: Int,
      contract: Contract,
      startDateTime: String,
      endDateTime: String,
      numberOfTicks: Int,
      whatToShow: String,
      useRTH: Int,
      ignoreSize: Boolean,
      miscOptions: List[TagValue]
  ) extends RequestMsg

  case ReqTickByTickData(
      msgId: Int = REQ_TICK_BY_TICK_DATA,
      reqId: Int,
      contract: Contract,
      tickType: TickType,
      numberOfTicks: Int,
      ignoreSize: Boolean
  ) extends RequestMsg

  case CancelTickByTickData(msgId: Int = CANCEL_TICK_BY_TICK_DATA, reqId: Int)
      extends RequestMsg

  case ReqCompletedOrders(msgId: Int = REQ_COMPLETED_ORDERS, apiOnly: Boolean)
      extends RequestMsg

  case ReqWshMetaData(msgId: Int = REQ_WSH_META_DATA, reqId: Int)
      extends RequestMsg

  case CancelWshMetaData(msgId: Int = CANCEL_WSH_META_DATA, reqId: Int)
      extends RequestMsg

  case ReqWshEventData(msgId: Int = REQ_WSH_EVENT_DATA, reqId: Int)
      extends RequestMsg

  case CancelWshEventData(msgId: Int = CANCEL_WSH_EVENT_DATA, reqId: Int)
      extends RequestMsg

  case ReqUserInf(msgId: Int = REQ_USER_INFO, reqId: Int) extends RequestMsg

  case ReqHeadTimestamp(
      msgId: Int = REQ_HEAD_TIMESTAMP,
      tickerId: Int,
      contract: Contract,
      whatToShow: String,
      useRTH: Int,
      formatDate: Int
  ) extends RequestMsg

  case CancelHeadTimestamp(msgId: Int = CANCEL_HEAD_TIMESTAMP, tickerId: Int)
      extends RequestMsg
