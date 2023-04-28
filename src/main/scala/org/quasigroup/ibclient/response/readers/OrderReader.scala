package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient

import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.*
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given

import cats.data.State
import cats.syntax.option.*

import scala.deriving.Mirror
//import shapeless3.deriving.*
//import record.*

object OrderReader {
  // Order().co
  // Tuple.fromProductTyped(Order())
  // //Tuple.fromProductTyped[Order](Order()).

  // def updateOrder[T](fieldName: String, value:T) = State[Tuple,Unit](tuple => (field ->> value) *: tuple -> ())
  def readBoxOrdrParams(version: Int): DecoderState[Option[Order.BoxOrderParams]] =
    if version >= 9 then read[Order.BoxOrderParams].map(_.some) else readNothing(None)

  def readPegToStkOrVolOrderParams(version: Int): DecoderState[Option[Order.PegToStkOrVolOrderParams]] =
    if version >= 9 then read[Order.PegToStkOrVolOrderParams].map(_.some) else readNothing(None)

  def readSmartComboRoutingParams(version: Int): DecoderState[List[TagValue]] =
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

  def readTrailParams(version: Int): DecoderState[Order.TrailParams] =
    for
      stopPrice <- if version >= 13 then read[Double] else readNothing(Double.MaxValue)
      trailingPercent <- if version >= 30 then read[Double] else readNothing(Double.MaxValue)
    yield Order.TrailParams(stopPrice = stopPrice, trailingPercent = trailingPercent)

  def readBasisPoints(version: Int): DecoderState[Option[Order.BasisPoints]] =
    if version >= 14 then read[Order.BasisPoints].map(_.some) else readNothing(None)

  def readFAParams(version: Int): DecoderState[Option[Order.FAParams]] =
    if version >= 7 then read[Order.FAParams].map(_.some) else readNothing(None)

  // def VolOrderParams(version: Int, readOpenOrderAttribs: Boolean): DecoderState[Option[Order.FAParams]] =
  //   // if version >= 7 then
  //   //   for
  //   //     faGroup <- read[String]
  //   //     faMethod <- read[Method]
  //   //     faPercentage <- read[String]
  //   //     faProfile <- read[String]
  //   //   yield Some(
  //   //     Order.FAParams(faGroup = faGroup, faMethod = faMethod, faPercentage = faPercentage, faProfile = faProfile)
  //   //   )
  //   else readNothing(None)

  def readContract(version: Int): DecoderState[Contract] =
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

  // val openOrder: DecoderState[Int] = {
  //     version <- read[Int]
  //     orderId <- read[Int]
  //     contract <- readContract(version)
  //     // read order fields
  //     action <- read[String]
  //     totalQuantity <- read[Decimal]
  //     orderType <- read[Order.Type]
  //     lmtPrice <- read[Double].map(double => if version < 29 && double == Double.MaxValue then 0 else double) // check version
  //     auxPrice <- read[Double].map(double => if version < 30 && double == Double.MaxValue then 0 else double) // check version
  //     tif <- read[String]
  //     ocaGroup <- read[String]
  //     account <- read[String]
  //     openClose <-  read[String]
  //     origin <- read[Int]
  //     orderRef <-  read[String]
  //     clientId <- if version >=3 then read[Int] else readNothing(-1)
  //     permId <- if version >=4 then read[Int] else readNothing(-1)
  //     outsideRth <- version >=4 then read[Boolean] else readNothing(false)
  //     hidden <- if version >=4 then read[Int].map(_==1) else readNothing(false)
  //     discretionaryAmt <- if version >=4 then read[Double] else readNothing(Double.MaxValue)
  //     goodAfterTime <-  if version >=5 then read[String] else readNothing("")
  //     _ <-  if version >=6 then read[String] else readNothing("")// eOrderDecoder.skipSharesAllocation();
  //     eOrderDecoder.readFAParams();
  //     modelCode <-  read[String]
  //     goodTillDate <-  if version >=8 then read[String] else readNothing("")
  //     rule80A <- if version >=9 then read[String] else readNothing("")
  //     percentOffset <- if version >=9 then read[Double] else readNothing(Double.MaxValue)
  //     settlingFirm <- if version >=9 then read[String] else readNothing("")
  //     eOrderDecoder.readShortSaleParams();
  //     auctionStrategy <- if version >=9 then read[String] else readNothing("")
  //     eOrderDecoder.readBoxOrderParams();
  //     eOrderDecoder.readPegToStkOrVolOrderParams();
  //     displaySize <-  if version >=9 then read[Int] else readNothing(Int.MaxValue)
  //     skipping eOrderDecoder.readOldStyleOutsideRth();
  //     blockOrder <- if version >=9 then read[Boolean] else readNothing(false)
  //     sweepToFill <- if version >=9 then read[Boolean] else readNothing(false)
  //     allOrNone <- if version >=9 then read[Boolean] else readNothing(false)
  //     minQty <-  if version >=9 then read[Int] else readNothing(Int.MaxValue)
  //     ocaType <-  if version >=9 then read[Int] else readNothing(0)
  //     _ <-  if version >=9 then read[Int] else readNothing(0) // eOrderDecoder.readETradeOnly(); // skip deprecated field
  //     _ <-  if version >=9 then read[Int] else readNothing(0) // eOrderDecoder.readFirmQuoteOnly();
  //     _ <-  if version >=9 then read[Int] else readNothing(0) //  eOrderDecoder.readNbboPriceCap();
  //     parentId <-  if version >=10 then read[Int] else readNothing(-1)
  //     triggerMethod <- if version >=10 then read[Int] else readNothing(0)
  //     eOrderDecoder.readVolOrderParams(true);
  //     trailParams<- readTrailParams(version)
  //     eOrderDecoder.readBasisPoints();
  //     eOrderDecoder.readComboLegs();
  //     comboLegsDescript <- if version >=14 then read[String] else readNothing("")
  //     eOrderDecoder.readSmartComboRoutingParams();
  //     eOrderDecoder.readScaleOrderParams();
  //     eOrderDecoder.readHedgeParams();
  //     optOutSmartRouting <-  version >=25 then read[Boolean] else readNothing(false)
  //     eOrderDecoder.readClearingParams();
  //     notHeld <- version >=22 then read[Boolean] else readNothing(false)
  //     eOrderDecoder.readDeltaNeutral();
  //     eOrderDecoder.readAlgoParams();
  //     solicited <-  if version >=33 then read[Boolean] else readNothing(false)
  //     eOrderDecoder.readWhatIfInfoAndCommission();
  //     eOrderDecoder.readVolRandomizeFlags();
  //     eOrderDecoder.readPegToBenchParams();
  //     eOrderDecoder.readConditions();
  //     eOrderDecoder.readAdjustedOrderParams();
  //     eOrderDecoder.readSoftDollarTier();
  //     cashQty <-  read[Double]
  //     eOrderDecoder.readDontUseAutoPriceForHedge();
  //     issOmsContainer <-  if version >=33 then read[Boolean] else readNothing(false)
  //     eOrderDecoder.readDiscretionaryUpToLimitPrice();
  //     eOrderDecoder.readUsePriceMgmtAlgo();
  //     eOrderDecoder.readDuration();
  //     eOrderDecoder.readPostToAts();
  //     eOrderDecoder.readAutoCancelParent(EClient.MIN_SERVER_VER_AUTO_CANCEL_PARENT);
  //     eOrderDecoder.readPegBestPegMidOrderAttributes();
  // }
  // val processCompletedOrderMsg = {
  //          eOrderDecoder.readContractFields();

  //     // read order fields
  //     eOrderDecoder.readAction();
  //     eOrderDecoder.readTotalQuantity();
  //     eOrderDecoder.readOrderType();
  //     eOrderDecoder.readLmtPrice();
  //     eOrderDecoder.readAuxPrice();
  //     eOrderDecoder.readTIF();
  //     eOrderDecoder.readOcaGroup();
  //     eOrderDecoder.readAccount();
  //     eOrderDecoder.readOpenClose();
  //     eOrderDecoder.readOrigin();
  //     eOrderDecoder.readOrderRef();
  //     eOrderDecoder.readPermId();
  //     eOrderDecoder.readOutsideRth();
  //     eOrderDecoder.readHidden();
  //     eOrderDecoder.readDiscretionaryAmount();
  //     eOrderDecoder.readGoodAfterTime();
  //     eOrderDecoder.readFAParams();
  //     eOrderDecoder.readModelCode();
  //     eOrderDecoder.readGoodTillDate();
  //     eOrderDecoder.readRule80A();
  //     eOrderDecoder.readPercentOffset();
  //     eOrderDecoder.readSettlingFirm();
  //     eOrderDecoder.readShortSaleParams();
  //     eOrderDecoder.readBoxOrderParams();
  //     eOrderDecoder.readPegToStkOrVolOrderParams();
  //     eOrderDecoder.readDisplaySize();
  //     eOrderDecoder.readSweepToFill();
  //     eOrderDecoder.readAllOrNone();
  //     eOrderDecoder.readMinQty();
  //     eOrderDecoder.readOcaType();
  //     eOrderDecoder.readTriggerMethod();
  //     eOrderDecoder.readVolOrderParams(false);
  //     eOrderDecoder.readTrailParams();
  //     eOrderDecoder.readComboLegs();
  //     eOrderDecoder.readSmartComboRoutingParams();
  //     eOrderDecoder.readScaleOrderParams();
  //     eOrderDecoder.readHedgeParams();
  //     eOrderDecoder.readClearingParams();
  //     eOrderDecoder.readNotHeld();
  //     eOrderDecoder.readDeltaNeutral();
  //     eOrderDecoder.readAlgoParams();
  //     eOrderDecoder.readSolicited();
  //     eOrderDecoder.readOrderStatus();
  //     eOrderDecoder.readVolRandomizeFlags();
  //     eOrderDecoder.readPegToBenchParams();
  //     eOrderDecoder.readConditions();
  //     eOrderDecoder.readStopPriceAndLmtPriceOffset();
  //     eOrderDecoder.readCashQty();
  //     eOrderDecoder.readDontUseAutoPriceForHedge();
  //     eOrderDecoder.readIsOmsContainer();
  //     eOrderDecoder.readAutoCancelDate();
  //     eOrderDecoder.readFilledQuantity();
  //     eOrderDecoder.readRefFuturesConId();
  //     eOrderDecoder.readAutoCancelParent();
  //     eOrderDecoder.readShareholder();
  //     eOrderDecoder.readImbalanceOnly();
  //     eOrderDecoder.readRouteMarketableToBbo();
  //     eOrderDecoder.readParentPermId();
  //     eOrderDecoder.readCompletedTime();
  //     eOrderDecoder.readCompletedStatus();
  //     eOrderDecoder.readPegBestPegMidOrderAttributes();

  //     m_EWrapper.completedOrder(contract, order, orderState);
  // }
}
