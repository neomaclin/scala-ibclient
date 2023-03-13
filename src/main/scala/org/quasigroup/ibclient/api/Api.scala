package org.quasigroup.ibclient.api

import org.quasigroup.ibclient.client.types.*
import org.quasigroup.ibclient.core.*
import org.quasigroup.ibclient.client.response.ResponseMsg
import org.quasigroup.ibclient.client.response.ResponseMsg.*
import fs2.Stream

trait Api[F[_]]:

  def connect(): F[ConnectionAck.type]
  
  def disConnect(): F[ConnectionClosed.type]

  def startAPI(): F[Unit]

  def reqScannerParameters(): F[ScannerParameters]

  def reqScannerSubscription(
      tickerId: Int,
      subscription: ScannerSubscription,
      scannerSubscriptionOptions: List[TagValue]
  ): Stream[F,ScannerData]

  def cancelScannerSubscription(tickerId: Int): F[ScannerDataEnd]

  def reqMktData(
      tickerId: Int,
      contract: Contract,
      genericTickList: String,
      snapshot: Boolean,
      mktDataOptions: List[TagValue]
  ): Stream[F, TickPrice | TickSize | TickString]

  def cancelMktData(tickerId: Int): F[TickSnapshotEnd]

//  def reqHistoricalData(
//      tickerId: Int,
//      contract: Contract,
//      endDateTime: String,
//      durationStr: String,
//      barSizeSetting: String,
//      whatToShow: String,
//      useRTH: Int,
//      formatDate: Int,
//      chartOptions: List[TagValue]
//  ): F[HistoricalData]
//
//  def cancelHistoricalData(tickerId: Int): F[Unit]

  def reqRealTimeBars(
      tickerId: Int,
      contract: Contract,
      barSize: Int,
      whatToShow: String,
      useRTH: Boolean,
      realTimeBarsOptions: List[TagValue]
  ): Stream[F, RealtimeBar]

  def cancelRealTimeBars(tickerId: Int): F[Unit]

  def reqContractDetails(reqId: Int, contract: Contract): Stream[F,ResponseMsg.ContractDetails]

  def reqMktDepth(
      tickerId: Int,
      contract: Contract,
      numRows: Int,
      mktDepthOptions: List[TagValue]
  ): F[UpdateMktDepth]

  def cancelMktDepth(tickerId: Int): F[Unit]

  def exerciseOptions(
      tickerId: Int,
      contract: Contract,
      exerciseAction: Int,
      exerciseQuantity: Int,
      account: String,
      `override`: Int
  ): F[Unit]

  def placeOrder(
      id: Int,
      contract: Contract,
      order: Order
  ): F[Unit]

  def cancelOrder(id: Int): F[Unit]

  def reqAccountUpdates(subscribe: Boolean, acctCode: String): F[Unit]

  def reqExecutions(reqId: Int, filter: ExecutionFilter): F[Unit]

  def reqOpenOrders(): F[Unit]

  def reqIds(numIds: Int): F[Unit]

  def reqNewsBulletins(allMsgs: Boolean): F[Unit]

  def cancelNewsBulletins: F[Unit]

  def setServerLogLevel(logleve: Int): F[Unit]

  def reqAutoOpenOrders(bAutoBind: Boolean): F[Unit]

  def reqAllOpenOrders: F[Unit]

  def reqManagedAccts: F[Unit]

  def requestFA(faDataType: Int): F[Unit]

  def replaceFA(faDataType: Int, xml: String): F[Unit]

  def reqCurrentTime: F[Unit]

  def reqFundamentalData(
      reqId: Int,
      contract: Contract,
      reportType: String
  ): F[Unit]

  def cancelFundamentalData(reqId: Int): F[Unit]

  def calculateImpliedVolatility(
      reqId: Int,
      contract: Contract,
      optionPrice: Double,
      underPrice: Double
  ): F[Unit]

  def cancelCalculateImpliedVolatility(reqId: Int): F[Unit]

  def calculateOptionPrice(
      reqId: Int,
      contract: Contract,
      volatility: Double,
      underPrice: Double
  ): F[Unit]

  def cancelCalculateOptionPrice(reqId: Int): F[Unit]

  def reqGlobalCancel: F[Unit]

  def reqMarketDataType(marketDataType: Int): F[Unit]

  def reqPositions: F[Unit]

  def cancelPositions: F[Unit]

  def reqAccountSummary(reqId: Int, group: String, tags: String): F[Unit]

  def cancelAccountSummary(reqId: Int): F[Unit]

  def verifyRequest(apiName: String, apiVersion: String): F[Unit]

  def verifyMessage(apiData: String): F[Unit]

  def verifyAndAuthRequest(
      apiName: String,
      apiVersion: String,
      opaqueIsvKey: String
  ): F[Unit]

  def verifyAndAuthMessage(
      apiData: String,
      xyzResponse: String
  ): F[Unit]

  def queryDisplayGroups(
      reqId: Int
  ): F[Unit]

  def subscribeToGroupEvents(reqId: Int, groupId: Int): F[Unit]

  def unsubscribeFromGroupEvents(reqId: Int): F[Unit]

  def updateDisplayGroup(reqId: Int, contractInfo: String): F[Unit]
