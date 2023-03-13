package org.quasigroup.ibclient.client.request

import MsgId.*
import org.quasigroup.ibclient.client.types.*
import org.quasigroup.ibclient.core.*

enum RequestMsg(msgId: Int, version: Int):
  case StartAPI(clientId: Int) extends RequestMsg(START_API, 2)

  case ReqScannerSubscription(
                               tickerId: Int,
                               subscription: ScannerSubscription,
                               scannerSubscriptionOptions: List[TagValue]
                             ) extends RequestMsg(REQ_SCANNER_SUBSCRIPTION, 4)

  case CancelScannerSubscription(tickerId: Int)
    extends RequestMsg(CANCEL_SCANNER_SUBSCRIPTION, 1)

  case ReqMktData(
                   tickerId: Int,
                   contract: Contract,
                   genericTickList: String,
                   snapshot: Boolean,
                   mktDataOptions: List[TagValue]
                 ) extends RequestMsg(REQ_MKT_DATA, 11)

  case CancelMktData(tickerId: Int) extends RequestMsg(CANCEL_MKT_DATA, 1)

  case ReqHistoricalData(
                          tickerId: Int,
                          contract: Contract,
                          endDateTime: String,
                          durationStr: String,
                          barSizeSetting: String,
                          whatToShow: String,
                          useRTH: Int,
                          formatDate: Int,
                          chartOptions: List[TagValue]
                        ) extends RequestMsg(REQ_HISTORICAL_DATA, 6)

  case CancelHistoricalData(tickerId: Int)
    extends RequestMsg(CANCEL_HISTORICAL_DATA, 1)

  case ReqRealTimeBars(
                        tickerId: Int,
                        contract: Contract,
                        barSize: Int,
                        whatToShow: String,
                        useRTH: Boolean,
                        realTimeBarsOptions: List[TagValue]
                      ) extends RequestMsg(REQ_REAL_TIME_BARS, 3)

  case CancelRealTimeBars(tickerId: Int)
    extends RequestMsg(CANCEL_REAL_TIME_BARS, 1)

  case ReqScannerParameters extends RequestMsg(REQ_SCANNER_PARAMETERS, 1)

  case ReqContractDetails(reqId: Int, contract: Contract)
    extends RequestMsg(REQ_CONTRACT_DATA, 8)

  case ReqMktDepth(
                    tickerId: Int,
                    contract: Contract,
                    numRows: Int,
                    mktDepthOptions: List[TagValue]
                  ) extends RequestMsg(REQ_MKT_DEPTH, 5)

  case CancelMktDepth(tickerId: Int) extends RequestMsg(CANCEL_MKT_DEPTH, 1)

  case ExerciseOptions(
                        tickerId: Int,
                        contract: Contract,
                        exerciseAction: Int,
                        exerciseQuantity: Int,
                        account: String,
                        `override`: Int
                      ) extends RequestMsg(EXERCISE_OPTIONS, 2)

  case PlaceOrder(
                   id: Int,
                   contract: Contract,
                   order: Order
                 ) extends RequestMsg(PLACE_ORDER, 44)

  case CancelOrder(id: Int) extends RequestMsg(CANCEL_ORDER, 1)

  case ReqAccountUpdates(subscribe: Boolean, acctCode: String)
    extends RequestMsg(REQ_ACCOUNT_DATA, 2)

  case ReqExecutions(reqId: Int, filter: ExecutionFilter)
    extends RequestMsg(REQ_EXECUTIONS, 3)

  case ReqOpenOrders extends RequestMsg(REQ_OPEN_ORDERS, 1)

  case ReqIds(numIds: Int) extends RequestMsg(REQ_IDS, 1)

  case ReqNewsBulletins(allMsgs: Boolean)
    extends RequestMsg(REQ_NEWS_BULLETINS, 1)

  case CancelNewsBulletins extends RequestMsg(CANCEL_NEWS_BULLETINS, 1)

  case SetServerLogLevel(logleve: Int)
    extends RequestMsg(SET_SERVER_LOGLEVEL, 1)

  case ReqAutoOpenOrders(bAutoBind: Boolean)
    extends RequestMsg(REQ_AUTO_OPEN_ORDERS, 1)

  case ReqAllOpenOrders extends RequestMsg(REQ_ALL_OPEN_ORDERS, 1)

  case ReqManagedAccts extends RequestMsg(REQ_MANAGED_ACCTS, 1)

  case RequestFA(faDataType: Int) extends RequestMsg(REQ_FA, 1)

  case ReplaceFA(faDataType: Int, xml: String) extends RequestMsg(REPLACE_FA, 1)

  case ReqCurrentTime extends RequestMsg(REQ_CURRENT_TIME, 1)

  case ReqFundamentalData(reqId: Int, contract: Contract, reportType: String)
    extends RequestMsg(REQ_FUNDAMENTAL_DATA, 2)

  case CancelFundamentalData(reqId: Int)
    extends RequestMsg(CANCEL_FUNDAMENTAL_DATA, 1)

  case CalculateImpliedVolatility(
                                   reqId: Int,
                                   contract: Contract,
                                   optionPrice: Double,
                                   underPrice: Double
                                 ) extends RequestMsg(REQ_CALC_IMPLIED_VOLAT, 2)

  case CancelCalculateImpliedVolatility(reqId: Int)
    extends RequestMsg(CANCEL_CALC_IMPLIED_VOLAT, 1)

  case CalculateOptionPrice(
                             reqId: Int,
                             contract: Contract,
                             volatility: Double,
                             underPrice: Double
                           ) extends RequestMsg(REQ_CALC_OPTION_PRICE, 2)

  case CancelCalculateOptionPrice(reqId: Int)
    extends RequestMsg(CANCEL_CALC_OPTION_PRICE, 1)

  case ReqGlobalCancel extends RequestMsg(REQ_GLOBAL_CANCEL, 1)

  case ReqMarketDataType(marketDataType: Int)
    extends RequestMsg(REQ_MARKET_DATA_TYPE, 1)

  case ReqPositions extends RequestMsg(REQ_POSITIONS, 1)

  case CancelPositions extends RequestMsg(CANCEL_POSITIONS, 1)

  case ReqAccountSummary(reqId: Int, group: String, tags: String)
    extends RequestMsg(REQ_ACCOUNT_SUMMARY, 1)

  case CancelAccountSummary(reqId: Int)
    extends RequestMsg(CANCEL_ACCOUNT_SUMMARY, 1)

  case VerifyRequest(apiName: String, apiVersion: String)
    extends RequestMsg(VERIFY_REQUEST, 1)

  case VerifyMessage(apiData: String) extends RequestMsg(VERIFY_MESSAGE, 1)

  case VerifyAndAuthRequest(
                             apiName: String,
                             apiVersion: String,
                             opaqueIsvKey: String
                           ) extends RequestMsg(VERIFY_AND_AUTH_REQUEST, 1)

  case VerifyAndAuthMessage(
                             apiData: String,
                             xyzResponse: String
                           ) extends RequestMsg(VERIFY_AND_AUTH_MESSAGE, 1)

  case QueryDisplayGroups(
                           reqId: Int
                         ) extends RequestMsg(QUERY_DISPLAY_GROUPS, 1)

  case SubscribeToGroupEvents(reqId: Int, groupId: Int)
    extends RequestMsg(SUBSCRIBE_TO_GROUP_EVENTS, 1)

  case UnsubscribeFromGroupEvents(reqId: Int)
    extends RequestMsg(UNSUBSCRIBE_FROM_GROUP_EVENTS, 1)

  case UpdateDisplayGroup(reqId: Int, contractInfo: String)
    extends RequestMsg(UPDATE_DISPLAY_GROUP, 1)

  case ReqHistogramData(
                         tickerId: Int,
                         contract: Contract,
                         useRTH: Boolean,
                         timePeriod: String
                       ) extends RequestMsg(REQ_HISTOGRAM_DATA, 0)


  case CancelHistogramData(tickerId: Int) extends RequestMsg(CANCEL_HISTOGRAM_DATA, 0)

  case ReqMarketRule(marketRuleId: Int) extends RequestMsg(REQ_MARKET_RULE, 0)

  case ReqPnL(reqId: Int, account: String, modelCode: String) extends RequestMsg(REQ_PNL, 0)

  case CancelPnL(reqId: Int) extends RequestMsg(CANCEL_PNL, 0)

  case ReqPnLSingle(reqId: Int, account: String, modelCode: String, conId: Int) extends RequestMsg(REQ_PNL_SINGLE, 0)

  case CancelPnLSingle(reqId: Int) extends RequestMsg(CANCEL_PNL_SINGLE, 0)

  case ReqHistoricalTicks(
                           reqId: Int,
                           contract: Contract,
                           startDateTime: String,
                           endDateTime: String,
                           numberOfTicks: Int,
                           whatToShow: String,
                           useRTH: Int,
                           ignoreSize: Boolean,
                           miscOptions: List[TagValue]
                         ) extends RequestMsg(REQ_HISTORICAL_TICKS, 0)

  case ReqTickByTickData(reqId: Int, contract: Contract, tickType: String, numberOfTicks: Int, ignoreSize: Boolean) extends RequestMsg(REQ_WSH_META_DATA, 0)

  case CancelTickByTickData(reqId: Int) extends RequestMsg(CANCEL_TICK_BY_TICK_DATA, 0)

  case ReqCompletedOrders(apiOnly: Boolean) extends RequestMsg(REQ_COMPLETED_ORDERS, 0)

  case ReqWshMetaData(reqId: Int) extends RequestMsg(REQ_WSH_META_DATA, 0)

  case CancelWshMetaData(reqId: Int) extends RequestMsg(CANCEL_WSH_META_DATA, 0)

  case ReqWshEventData(reqId: Int) extends RequestMsg(REQ_WSH_EVENT_DATA, 0)

  case CancelWshEventData(reqId: Int) extends RequestMsg(CANCEL_WSH_EVENT_DATA, 0)

  case ReqUserInf(reqId: Int) extends RequestMsg(REQ_USER_INFO, 0)