package org.quasigroup.ibclient.types

final case class BitMask(mask: Int) {
  def clear: BitMask = BitMask(0)

  def get(index: Int): Boolean = (mask & (1 << index)) != 0

  def set(index: Int, element: Boolean): (BitMask, Boolean) =
    (
      if element then BitMask(mask | (1 << index))
      else BitMask(mask & ~(1 << index)),
      get(index)
    )

}

final case class TagValue(tag: String, value: String)

final case class ComboLeg(
    conId: Int,
    ratio: Int,
    action: Action,
    exchange: String,
    openClose: ComboLeg.OpenClose,
    shortSaleSlot: Int,
    designatedLocation: String,
    exemptCode: Int
)

final case class Contract(
    conId: Int = -1,
    symbol: String,
    secType: SecType,
    lastTradeDateOrContractMonth: String = "",
    strike: Double = 0,
    right: ContractRight = ContractRight.None,
    multiplier: String = "",
    exchange: String = "",
    currency: String = "",
    localSymbol: String = "",
    tradingClass: String = "",
    comboLegs: List[ComboLeg] = Nil,
    primaryExch: String = "",
    includeExpired: Boolean = false,
    secIdType: SecIdType = SecIdType.None,
    secId: String = "",
    description: String = "",
    issuerId: String = "",
    comboLegsDescrip: String = "",
    deltaNeutralContract: Option[DeltaNeutralContract] = None
)

final case class ContractDetails(
    contract: Contract,
    marketName: String = "",
    minTick: Double = Double.MaxValue,
    priceMagnifier: Int = 0,
    orderTypes: String = "",
    validExchanges: String = "",
    underConId: Int = -1,
    longName: String = "",
    contractMonth: String = "",
    industry: String = "",
    category: String = "",
    subcategory: String = "",
    timeZoneId: String = "",
    tradingHours: String = "",
    liquidHours: String = "",
    evRule: String = "",
    evMultiplier: Double = Double.MaxValue,
    secIdList: List[TagValue] = Nil,
    aggGroup: Int = 0,
    underSymbol: String = "",
    underSecType: SecType = SecType.None,
    marketRuleIds: String = "",
    realExpirationDate: String = "",
    lastTradeTime: String = "",
    stockType: String = "",
    minSize: Decimal = Decimal.INVALID,
    sizeIncrement: Decimal = Decimal.INVALID,
    suggestedSizeIncrement: Decimal = Decimal.INVALID,
    // BOND VALUES
    cusip: String = "",
    ratings: String = "",
    descAppend: String = "",
    bondType: String = "",
    couponType: String = "",
    callable: Boolean = false,
    putable: Boolean = false,
    coupon: Double = 0,
    convertible: Boolean = false,
    maturity: String = "",
    issueDate: String = "",
    nextOptionDate: String = "",
    nextOptionType: String = "",
    nextOptionPartial: Boolean = false,
    notes: String = ""
)
final case class ContractDescription(
    contract: Contract,
    derivativeSecTypes: List[String]
)
final case class DeltaNeutralContract(conid: Int, delta: Double, price: Double)
final case class ScannerDataElement(
    rank: Int,
    contractDetails: ContractDetails,
    distance: String,
    benchmark: String,
    projection: String,
    legsStr: String
)

object ComboLeg:
  enum OpenClose:
    case Same, Open, Close, Unknown
end ComboLeg

enum ComboParam:
  case NonGuaranteed, PriceCondConid, CondPriceMax, CondPriceMin,
    ChangeToMktTime1, ChangeToMktTime2, DiscretionaryPct, DontLeginNext,
    LeginPrio, MaxSegSize
end ComboParam

enum AlgoParam:
  case startTime, endTime, allowPastEndTime, maxPctVol, pctVol, strategyType,
    noTakeLiq, riskAversion, forceCompletion, displaySize, getDone,
    noTradeAhead, useOddLots,
    componentSize, timeBetweenOrders, randomizeTime20, randomizeSize55, giveUp,
    catchUp, waitForFill, activeTimeStart, activeTimeEnd
end AlgoParam

import AlgoParam.*
enum AlgoStrategy(val params: List[AlgoParam]):

  case None extends AlgoStrategy(Nil)
  case Vwap
      extends AlgoStrategy(
        List(
          startTime,
          endTime,
          maxPctVol,
          noTakeLiq,
          getDone,
          noTradeAhead,
          useOddLots
        )
      )
  case Twap
      extends AlgoStrategy(
        List(
          startTime,
          endTime,
          allowPastEndTime,
          strategyType
        )
      )
  case ArrivalPx
      extends AlgoStrategy(
        List(
          startTime,
          endTime,
          allowPastEndTime,
          maxPctVol,
          riskAversion,
          forceCompletion
        )
      )
  case DarkIce
      extends AlgoStrategy(
        List(
          startTime,
          endTime,
          allowPastEndTime,
          displaySize
        )
      )
  case PctVol
      extends AlgoStrategy(
        List(
          startTime,
          endTime,
          pctVol,
          noTakeLiq
        )
      )
  case AD
      extends AlgoStrategy(
        List(
          activeTimeStart,
          activeTimeEnd,
          componentSize,
          timeBetweenOrders,
          randomizeTime20,
          randomizeSize55,
          giveUp,
          catchUp,
          waitForFill
        )
      )
end AlgoStrategy

enum HedgeType:
  case None, Delta, Beta, Fx, Pair

enum ContractRight:
  case None, Put, Call

object ContractRight:
  def fromString(str: String) =
    str.headOption
      .collect {
        case 'P' => Put
        case 'C' => Call
      }
      .getOrElse(None)

enum VolatilityType:
  case None, Daily, Annual

enum ReferencePriceType:
  case None, Midpoint, BidOrAsk

enum TriggerMethod(val value: Int, val name: String):
  case Default extends TriggerMethod(0, "default")
  case DoubleBidAsk extends TriggerMethod(1, "double bid/ask")
  case Last extends TriggerMethod(2, "last")
  case DoubleLast extends TriggerMethod(3, "double last")
  case BidAsk extends TriggerMethod(4, "bid/ask")
  case LastOrBidAsk extends TriggerMethod(7, "last of bid/ask")
  case Midpoint extends TriggerMethod(8, "mid-point")
end TriggerMethod

enum Action:
  case BUY, SELL, SSHORT

enum Rule80A(val value: String):
  case None extends Rule80A("")
  case IndivArb extends Rule80A("J")
  case IndivBigNonArb extends Rule80A("K")
  case IndivSmallNonArb extends Rule80A("I")
  case INST_ARB extends Rule80A("U")
  case InstBigNonArb extends Rule80A("Y")
  case InstSmallNonArb extends Rule80A("A")
end Rule80A

enum OcaType:
  case None, CancelWithBlocking, ReduceWithBlocking, ReduceWithoutBlocking

enum TimeInForce:
  case DAY, GTC, OPG, IOC, GTD, GTT, AUC, FOK, GTX, DTC

enum ExerciseType:
  case None, Exercise, Lapse

enum FundamentalType:
  case ReportSnapshot, ReportsFinSummary, ReportRatios, ReportsFinStatements,
    RESC, CalendarReport

enum WhatToShow:
  case TRADES, MIDPOINT, BID,
    ASK, // << only these are valid for real-time bars
    BID_ASK, HISTORICAL_VOLATILITY, OPTION_IMPLIED_VOLATILITY, YIELD_ASK,
    YIELD_BID, YIELD_BID_ASK, YIELD_LAST

enum BarSize:
  case _1_secs, _5_secs, _10_secs, _15_secs, _30_secs, _1_min, _2_mins,
    _3_mins, _5_mins, _10_mins, _15_mins, _20_mins, _30_mins, _1_hour,
    _4_hours, _1_day, _1_week

enum DurationUnit:
  case SECOND, DAY, WEEK, MONTH, YEAR

enum DeepType:
  case INSERT, UPDATE, DELETE

enum DeepSide:
  case SELL, BUY

enum NewsType:
  case UNKNOWN, BBS, LIVE_EXCH, DEAD_EXCH, HTML, POPUP_TEXT, POPUP_HTML

enum FADataType:
  case UNUSED, GROUPS, PROFILES, ALIASES

enum SecIdType:
  case None, CUSIP, SEDOL, ISIN, RIC

enum SecType:
  case None, STK, OPT, FUT, CASH, BOND, CFD, FOP, WAR, IOPT, FWD, BAG, IND,
    BILL, FUND, FIXED, SLB, NEWS, CMDTY, BSK, ICU, ICS, CRYPTO

enum MktDataType:
  case Unknown, Realtime, Frozen, Delayed, DelayedFrozen

enum AuctionStrategy:
  case None, AUCTION_MATCH, AUCTION_IMPROVEMENT, AUCTION_TRANSPARENT

final case class DepthMktDataDescription(
    exchange: String,
    secType: SecType,
    listingExch: String,
    serviceDataType: String,
    aggGroup: Int
)

enum Method:
  case None, EqualQuantity, AvailableEquity, NetLiq, PctChange

enum TickType(val index: Int, field: String):
  case BID_SIZE extends TickType(0, "bidSize")
  case BID extends TickType(1, "bidPrice")
  case ASK extends TickType(2, "askPrice")
  case ASK_SIZE extends TickType(3, "askSize")
  case LAST extends TickType(4, "lastPrice")
  case LAST_SIZE extends TickType(5, "lastSize")
  case HIGH extends TickType(6, "high")
  case LOW extends TickType(7, "low")
  case VOLUME extends TickType(8, "volume")
  case CLOSE extends TickType(9, "close")
  case BID_OPTION extends TickType(10, "bidOptComp")
  case ASK_OPTION extends TickType(11, "askOptComp")
  case LAST_OPTION extends TickType(12, "lastOptComp")
  case MODEL_OPTION extends TickType(13, "modelOptComp")
  case OPEN extends TickType(14, "open")
  case LOW_13_WEEK extends TickType(15, "13WeekLow")
  case HIGH_13_WEEK extends TickType(16, "13WeekHigh")
  case LOW_26_WEEK extends TickType(17, "26WeekLow")
  case HIGH_26_WEEK extends TickType(18, "26WeekHigh")
  case LOW_52_WEEK extends TickType(19, "52WeekLow")
  case HIGH_52_WEEK extends TickType(20, "52WeekHigh")
  case AVG_VOLUME extends TickType(21, "AvgVolume")
  case OPEN_INTEREST extends TickType(22, "OpenInterest")
  case OPTION_HISTORICAL_VOL extends TickType(23, "OptionHistoricalVolatility")
  case OPTION_IMPLIED_VOL extends TickType(24, "OptionImpliedVolatility")
  case OPTION_BID_EXCH extends TickType(25, "OptionBidExchStr")
  case OPTION_ASK_EXCH extends TickType(26, "OptionAskExchStr")
  case OPTION_CALL_OPEN_INTEREST extends TickType(27, "OptionCallOpenInterest")
  case OPTION_PUT_OPEN_INTEREST extends TickType(28, "OptionPutOpenInterest")
  case OPTION_CALL_VOLUME extends TickType(29, "OptionCallVolume")
  case OPTION_PUT_VOLUME extends TickType(30, "OptionPutVolume")
  case INDEX_FUTURE_PREMIUM extends TickType(31, "IndexFuturePremium")
  case BID_EXCH extends TickType(32, "bidExch") // string
  case ASK_EXCH extends TickType(33, "askExch") // string
  case AUCTION_VOLUME extends TickType(34, "auctionVolume")
  case AUCTION_PRICE extends TickType(35, "auctionPrice")
  case AUCTION_IMBALANCE extends TickType(36, "auctionImbalance")
  case MARK_PRICE extends TickType(37, "markPrice")
  case BID_EFCOMPUTATION extends TickType(38, "bidEFP")
  case ASK_EFCOMPUTATION extends TickType(39, "askEFP")
  case LAST_EFCOMPUTATION extends TickType(40, "lastEFP")
  case OPEN_EFCOMPUTATION extends TickType(41, "openEFP")
  case HIGH_EFCOMPUTATION extends TickType(42, "highEFP")
  case LOW_EFCOMPUTATION extends TickType(43, "lowEFP")
  case CLOSE_EFCOMPUTATION extends TickType(44, "closeEFP")
  case LAST_TIMESTAMP extends TickType(45, "lastTimestamp") // string
  case SHORTABLE extends TickType(46, "shortable")
  case FUNDAMENTAL_RATIOS extends TickType(47, "fundamentals") // string
  case RT_VOLUME extends TickType(48, "RTVolume") // string
  case HALTED extends TickType(49, "halted")
  case BID_YIELD extends TickType(50, "bidYield")
  case ASK_YIELD extends TickType(51, "askYield")
  case LAST_YIELD extends TickType(52, "lastYield")
  case CUST_OPTION_COMPUTATION extends TickType(53, "custOptComp")
  case TRADE_COUNT extends TickType(54, "trades")
  case TRADE_RATE extends TickType(55, "trades/min")
  case VOLUME_RATE extends TickType(56, "volume/min")
  case LAST_RTH_TRADE extends TickType(57, "lastRTHTrade")
  case RT_HISTORICAL_VOL extends TickType(58, "RTHistoricalVol")
  case REGULATORY_IMBALANCE extends TickType(61, "regulatoryImbalance")
  case NEWS_TICK extends TickType(62, "newsTick")
  case SHORT_TERVOLUME_3_MIN extends TickType(63, "shortTermVolume3Min")
  case SHORT_TERVOLUME_5_MIN extends TickType(64, "shortTermVolume5Min")
  case SHORT_TERVOLUME_10_MIN extends TickType(65, "shortTermVolume10Min")
  case DELAYED_BID extends TickType(66, "delayedBid")
  case DELAYED_ASK extends TickType(67, "delayedAsk")
  case DELAYED_LAST extends TickType(68, "delayedLast")
  case DELAYED_BID_SIZE extends TickType(69, "delayedBidSize")
  case DELAYED_ASK_SIZE extends TickType(70, "delayedAskSize")
  case DELAYED_LAST_SIZE extends TickType(71, "delayedLastSize")
  case DELAYED_HIGH extends TickType(72, "delayedHigh")
  case DELAYED_LOW extends TickType(73, "delayedLow")
  case DELAYED_VOLUME extends TickType(74, "delayedVolume")
  case DELAYED_CLOSE extends TickType(75, "delayedClose")
  case DELAYED_OPEN extends TickType(76, "delayedOpen")
  case RT_TRD_VOLUME extends TickType(77, "rtTrdVolume")
  case CREDITMAN_MARK_PRICE extends TickType(78, "creditmanMarkPrice")
  case CREDITMAN_SLOW_MARK_PRICE extends TickType(79, "creditmanSlowMarkPrice")
  case DELAYED_BID_OPTION extends TickType(80, "delayedBidOptComp")
  case DELAYED_ASK_OPTION extends TickType(81, "delayedAskOptComp")
  case DELAYED_LAST_OPTION extends TickType(82, "delayedLastOptComp")
  case DELAYED_MODEL_OPTION extends TickType(83, "delayedModelOptComp")
  case LAST_EXCH extends TickType(84, "lastExchange")
  case LAST_REG_TIME extends TickType(85, "lastRegTime")
  case FUTURES_OPEN_INTEREST extends TickType(86, "futuresOpenInterest")
  case AVG_OPT_VOLUME extends TickType(87, "avgOptVolume")
  case DELAYED_LAST_TIMESTAMP extends TickType(88, "delayedLastTimestamp")
  case SHORTABLE_SHARES extends TickType(89, "shortableShares")
  case DELAYED_HALTED extends TickType(90, "delayedHalted")
  case REUTERS_2_MUTUAL_FUNDS extends TickType(91, "reuters2MutualFunds")
  case ETF_NAV_CLOSE extends TickType(92, "etfNavClose")
  case ETF_NAV_PRIOR_CLOSE extends TickType(93, "etfNavPriorClose")
  case ETF_NAV_BID extends TickType(94, "etfNavBid")
  case ETF_NAV_ASK extends TickType(95, "etfNavAsk")
  case ETF_NAV_LAST extends TickType(96, "etfNavLast")
  case ETF_FROZEN_NAV_LAST extends TickType(97, "etfFrozenNavLast")
  case ETF_NAV_HIGH extends TickType(98, "etfNavHigh")
  case ETF_NAV_LOW extends TickType(99, "etfNavLow")
  case SOCIAL_MARKET_ANALYTICS extends TickType(100, "socialMarketAnalytics")
  case ESTIMATED_IPO_MIDPOINT extends TickType(101, "estimatedIPOMidpoint")
  case FINAL_IPO_LAST extends TickType(102, "finalIPOLast")
  case UNKNOWN extends TickType(Int.MaxValue, "unknown")

final case class CommissionReport(
    execId: String,
    commission: Double,
    currency: String,
    realizedPNL: Double,
    `yield`: Double,
    yieldRedemptionDat: Int
)

enum Liquidities:
  case None, Added, Removed, RoudedOut

final case class Execution(
    orderId: Int = -1,
    clientId: Int = -1,
    execId: String = "",
    time: String = "",
    acctNumber: String = "",
    exchange: String = "",
    side: String = "",
    shares: Decimal = Decimal.ZERO,
    price: Double = 0.0,
    permId: Int = -1,
    liquidation: Int = 0,
    cumQty: Decimal = Decimal.ZERO,
    avgPrice: Double = 0.0,
    orderRef: String = "",
    evRule: String = "",
    evMultiplier: Double = 0.0,
    modelCode: String = "",
    lastLiquidity: Liquidities = Liquidities.None
)

final case class ExecutionFilter(
    clientId: Int,
    acctCode: String,
    time: String,
    symbol: String,
    secType: SecType,
    exchange: String,
    side: String
)

opaque type ComboContract = Contract
opaque type FutContract = Contract
opaque type OptContract = Contract
opaque type StkContract = Contract

final case class ScannerSubscription(
    numberOfRows: Int,
    instrument: String,
    locationCode: String,
    scanCode: ScanCode,
    abovePrice: Double,
    belowPrice: Double,
    aboveVolume: Int,
    averageOptionVolumeAbove: Int,
    marketCapAbove: Double,
    marketCapBelow: Double,
    moodyRatingAbove: String,
    moodyRatingBelow: String,
    spRatingAbove: String,
    spRatingBelow: String,
    maturityDateAbove: String,
    maturityDateBelow: String,
    couponRateAbove: Double,
    couponRateBelow: Double,
    excludeConvertible: String,
    scannerSettingPairs: String,
    stockTypeFilter: String
)

final case class Order(
    // ids
    clientId: Int = -1,
    orderId: Int = -1,
    permId: Int = -1,
    parentId: Int = -1,
    // primary attributes
    action: Action = Action.BUY,
    totalQuantity: Decimal = Decimal.INVALID,
    displaySize: Int = 0,
    orderType: Order.Type = Order.Type.LMT,
    lmtPrice: Double = Double.MaxValue,
    auxPrice: Double = Double.MaxValue,
    tif: TimeInForce = TimeInForce.DAY,
    // Clearing info
    account: String = "",
    settlingFirm: String = "",
    clearingAccount: String = "",
    clearingIntent: String = "",
    // secondary attributes
    allOrNone: Boolean = false,
    blockOrder: Boolean = false,
    hidden: Boolean = false,
    outsideRth: Boolean = false,
    sweepToFill: Boolean = false,
    percentOffset: Double = Double.MaxValue,
    trailingParams: Order.TrailParams,
    minQty: Int = Int.MaxValue,
    goodAfterTime: String, // FORMAT: 20060505 08:00:00 EST
    goodTillDate: String, // FORMAT: 20060505 08:00:00 EST or 20060505
    ocaGroup: String = "",
    orderRef: String = "",
    rule80A: Rule80A = Rule80A.None,
    ocaType: OcaType = OcaType.None,
    triggerMethod: TriggerMethod = TriggerMethod.Default,
    // extended order fields
    scaleTable: String = "",
    activeStartTime: String = "", // GTC orders
    activeStopTime: String = "", // GTC orders
    // advisor allocation orders
    faFields: Option[Order.FAParams] = None,
    // volatility orders
    volOrderFields: Option[Order.VolOrderParams] = None,
    scaleFields: Order.ScaleOrderParams,

    // hedge orders
    hedgeFields: Option[Order.HedgeParams],
    // algo orders
    algoFields: Option[Order.AlgoParams],
    // // combo orders
    smartComboRoutingFields: List[TagValue] = Nil,
    orderComboLegs: List[Order.ComboLeg] = Nil,
    // processing control
    whatIf: Boolean = false,
    transmit: Boolean = true, // if false, order will be sent to TWS but not transmitted to server
    overridePercentageConstraints: Boolean = false,
    // Institutional orders only
    openClose: String = "O", // O=Open, C=Close
    origin: Int = Order.CUSTOMER,
    shortSaleFields: Option[Order.ShortSaleParams] = None,
    // SMART routing only
    discretionaryAmt: Double = Double.MaxValue,
    optOutSmartRouting: Boolean = false,
    // BOX or VOL ORDERS ONLY
    auctionStrategy: AuctionStrategy = AuctionStrategy.None,
    // BOX ORDERS ONLY
    boxOrderFields: Option[Order.BoxOrderParams],
    // pegged to stock or VOL orders
    pegToStkOrVolOrderFields: Option[Order.PegToStkOrVolOrderParams],
    // COMBO ORDERS ONLY
    basisPoints: Option[Order.BasisPoints] = None,
    // Not Held
    notHeld: Boolean = false,
    // order misc options
    orderMiscOptions: List[TagValue] = Nil,
    // order algo id
    solicited: Boolean = false,
    randomizeSize: Boolean = false,
    randomizePrice: Boolean = false,
    // VER PEG2BENCH fields:
    pegToBenchFields: Option[Order.PegToBenchParams] = None,
    adjustedOrderType: Order.Type = Order.Type.None,
    triggerPrice: Double = Double.MaxValue,
    trailStopPrice: Double = Double.MaxValue,
    lmtPriceOffset: Double = Double.MaxValue,
    adjustedStopPrice: Double = Double.MaxValue,
    adjustedStopLimitPrice: Double = Double.MaxValue,
    adjustedTrailingAmount: Double = Double.MaxValue,
    adjustableTrailingUnit: Int = 0,
    conditionFields: Option[Order.ConditionParams] = None,
    // VER PEG2BENCH fields ends:
    // models
    modelCode: String = "",
    extOperator: String = "",
    softDollarTier: SoftDollarTier = new SoftDollarTier("", "", ""),
    // native cash quantity
    cashQty: Double = Double.MaxValue,
    mifid2DecisionMaker: String = "",
    mifid2DecisionAlgo: String = "",
    mifid2ExecutionTrader: String = "",
    mifid2ExecutionAlgo: String = "",
    // don't use auto price for hedge
    dontUseAutoPriceForHedge: Boolean = false,
    isOmsContainer: Boolean = false,
    discretionaryUpToLimitPrice: Boolean = false,
    autoCancelDate: String = "",
    filledQuantity: Decimal = Decimal.INVALID,
    refFuturesConId: Int = -1,
    autoCancelParent: Boolean = false,
    shareholder: String = "",
    imbalanceOnly: Boolean = false,
    routeMarketableToBbo: Boolean = false,
    parentPermId: Long = -1L,
    usePriceMgmtAlgo: UsePriceMgmtAlgo = UsePriceMgmtAlgo.None,
    duration: Int = Int.MaxValue,
    postToAts: Int = Int.MaxValue,
    advancedErrorOverride: String = "",
    manualOrderTime: String = "",
    minTradeQty: Int = Int.MaxValue,
    minCompeteSize: Int = Int.MaxValue,
    competeAgainstBestOffset: Double = Double.MaxValue,
    midOffsetAtWhole: Double = Double.MaxValue,
    midOffsetAtHalf: Double = Double.MaxValue
) {
  def isCompeteAgainstBestOffsetUpToMid: Boolean =
    competeAgainstBestOffset == Order.COMPETE_AGAINST_BEST_OFFSET_UP_TO_MID
}

object Order:
  val CUSTOMER = 0
  val FIRM = 1
  val OPT_UNKNOWN = '?'
  val OPT_BROKER_DEALER = 'b'
  val OPT_CUSTOMER = 'c'
  val OPT_FIRM = 'f'
  val OPT_ISEMM = 'm'
  val OPT_FARMM = 'n'
  val OPT_SPECIALIST = 'y'
  val AUCTION_MATCH = 1
  val AUCTION_IMPROVEMENT = 2
  val AUCTION_TRANSPARENT = 3
  val COMPETE_AGAINST_BEST_OFFSET_UP_TO_MID = Double.PositiveInfinity

  enum Type(val apiString: String):
    case None extends Type("")
    case MKT extends Type("MKT")
    case LMT extends Type("LMT")
    case STP extends Type("STP")
    case STLMT extends Type("STP LMT")
    case REL extends Type("REL")
    case TRAIL extends Type("TRAIL")
    case BOX_TOP extends Type("BOX TOP")
    case FIX_PEGGED extends Type("FIX PEGGED")
    case LIT extends Type("LIT")
    case LMT_PLUS_MKT extends Type("LMT + MKT")
    case LOC extends Type("LOC")
    case MIT extends Type("MIT")
    case MKT_PRT extends Type("MKT PRT")
    case MOC extends Type("MOC")
    case MTL extends Type("MTL")
    case PASSV_REL extends Type("PASSV REL")
    case PEG_BENCH extends Type("PEG BENCH")
    case PEG_BEST extends Type("PEG BEST")
    case PEG_MID extends Type("PEG MID")
    case PEG_MKT extends Type("PEG MKT")
    case PEG_PRIM extends Type("PEG PRIM")
    case PEG_STK extends Type("PEG STK")
    case REL_PLUS_LMT extends Type("REL + LMT")
    case REL_PLUS_MKT extends Type("REL + MKT")
    case STPRT extends Type("STP PRT")
    case TRAIL_LIMIT extends Type("TRAIL LIMIT")
    case TRAIL_LIT extends Type("TRAIL LIT")
    case TRAIL_LMT_PLUS_MKT extends Type("TRAIL LMT + MKT")
    case TRAIL_MIT extends Type("TRAIL MIT")
    case TRAIL_REL_PLUS_MKT extends Type("TRAIL REL + MKT")
    case VOL extends Type("VOL")
    case VWAP extends Type("VWAP")
    case QUOTE extends Type("QUOTE")
    case PEG_PRIM_VOL extends Type("PPV")
    case PEG_MID_VOL extends Type("PDV")
    case PEG_MKT_VOL extends Type("PMV")
    case PEG_SRF_VOL extends Type("PSV")
  end Type

  final case class ComboLeg(price: Double)

  final case class State(
      status: Order.Status = Order.Status.Unknown,
      initMarginBefore: String = "",
      maintMarginBefore: String = "",
      equityWithLoanBefore: String = "",
      initMarginChange: String = "",
      maintMarginChange: String = "",
      equityWithLoanChange: String = "",
      initMarginAfter: String = "",
      maintMarginAfter: String = "",
      equityWithLoanAfter: String = "",
      commission: Double = Double.MaxValue,
      minCommission: Double = Double.MaxValue,
      maxCommission: Double = Double.MaxValue,
      commissionCurrency: String = "",
      warningText: String = "",
      completedTime: String = "",
      completedStatus: Status = Order.Status.Unknown
  )

  enum Status:
    case ApiPending,
      ApiCancelled,
      PreSubmitted,
      PendingCancel,
      Cancelled,
      Submitted,
      Filled,
      Inactive,
      PendingSubmit,
      Unknown

    def isActive(order: Order.Status): Boolean =
      order == PreSubmitted || order == PendingCancel || order == Submitted || order == PendingSubmit
  end Status

  final case class BasisPoints(value: Double = Double.MaxValue, `type`: Int = Int.MaxValue)

  final case class VolOrderParams(
      volatility: Double = Double.MaxValue,
      volatilityType: VolatilityType = VolatilityType.None,
      continuousUpdate: Int = 0,
      referencePriceType: ReferencePriceType,
      deltaNeutralOrderType: Order.Type = Order.Type.None,
      deltaNeutralAuxPrice: Double = Double.MaxValue,
      deltaNeutralConId: Int = -1,
      deltaNeutralSettlingFirm: String = "",
      deltaNeutralClearingAccount: String = "",
      deltaNeutralClearingIntent: String = "",
      deltaNeutralOpenClose: String = "",
      deltaNeutralShortSale: Boolean = false,
      deltaNeutralShortSaleSlot: Int = 0,
      deltaNeutralDesignatedLocation: String = ""
  )

  final case class TrailParams(stopPrice: Double = Double.MaxValue, trailingPercent: Double = Double.MaxValue)

  final case class PegToBenchParams(
      referenceContractId: Int = -1,
      isPeggedChangeAmountDecrease: Boolean = false,
      peggedChangeAmount: Double = 0.0,
      referenceChangeAmount: Double = 0.0,
      referenceExchangeId: String = ""
  )
  final case class ScaleOrderParams(
      scaleInitLevelSize: Int = Int.MaxValue,
      scaleSubsLevelSize: Int = Int.MaxValue,
      scalePriceIncrement: Double = Double.MaxValue,
      scalePriceAdjustValue: Double = Double.MaxValue,
      scalePriceAdjustInterval: Int = Int.MaxValue,
      scaleProfitOffset: Double = Double.MaxValue,
      scaleAutoReset: Boolean = false,
      scaleInitPosition: Int = Int.MaxValue,
      scaleInitFillQty: Int = Int.MaxValue,
      scaleRandomPercent: Boolean = false
  )

  final case class FAParams(
      faGroup: String = "",
      faMethod: Method = Method.None,
      faPercentage: String = ""
  )

  final case class HedgeParams(`type`: HedgeType = HedgeType.None, value: String = "")

  final case class ClearingParams(account: String = "", intent: String = "")

  final case class AlgoParams(strategy: AlgoStrategy = AlgoStrategy.None, params: List[TagValue] = Nil, id: String = "")

  final case class AdjustedOrderParams(
      adjustedOrderType: Order.Type = Order.Type.None,
      triggerPrice: Double = Double.MaxValue,
      adjustedStopPrice: Double = Double.MaxValue,
      adjustedStopLimitPrice: Double = Double.MaxValue,
      adjustedTrailingAmount: Double = Double.MaxValue,
      adjustableTrailingUnit: Int = 0
  )

  final case class PegToStkOrVolOrderParams(
      stockRangeLower: Double = Double.MaxValue,
      stockRangeUpper: Double = Double.MaxValue
  )
  final case class PegBestPegMidOrderAttributes(
      minTradeQty: Int = Int.MaxValue,
      minCompeteSize: Int = Int.MaxValue,
      competeAgainstBestOffset: Double = Double.MaxValue,
      midOffsetAtWhole: Double = Double.MaxValue,
      midOffsetAtHalf: Double = Double.MaxValue,
      allOrNone: Boolean = false
  )

  final case class BoxOrderParams(
      startingPrice: Double = Double.MaxValue,
      stockRefPrice: Double = Double.MaxValue,
      delta: Double = Double.MaxValue
  )

  final case class ConditionParams(
      conditions: List[OrderCondition] = Nil,
      conditionsIgnoreRth: Boolean = false,
      conditionsCancelOrder: Boolean = false
  )

  final case class ShortSaleParams(
      shortSaleSlot: Int = 0,
      // 1 if you hold the shares, 2 if they will be delivered from elsewhere.  Only for Action="SSHORT
      designatedLocation: String = "",
      exemptCode: Int = -1
  )

end Order

final case class TickAttrib(
    canAutoExecute: Boolean = false,
    pastLimit: Boolean = false,
    preOpen: Boolean = false
)
final case class TickAttribBidAsk(
    bidPastLow: Boolean = false,
    askPastHigh: Boolean = false
)

final case class TickAttribLast(
    pastLimit: Boolean = false,
    unreported: Boolean = false
)

final case class HistogramEntry(price: Double, size: Decimal)

final case class HistoricalSession(
    startDateTime: String,
    endDateTime: String,
    refDate: String
)

final case class HistoricalTick(time: Long, price: Double, size: Decimal)

final case class HistoricalTickBidAsk(
    time: Long,
    tickAttribBidAsk: TickAttribBidAsk,
    priceBid: Double,
    priceAsk: Double,
    sizeBid: Decimal,
    sizeAsk: Decimal
)

final case class HistoricalTickLast(
    time: Long,
    tickAttribLast: TickAttribLast,
    price: Double,
    size: Decimal,
    exchange: String,
    specialConditions: String
)

final case class SoftDollarTier(
    name: String,
    value: String,
    displayName: String
)

final case class FamilyCode(accountID: String, familyCodeStr: String)

final case class NewsProvider(providerCode: String, providerName: String)

final case class PriceIncrement(lowEdge: Double, increment: Double)

enum UsePriceMgmtAlgo(val value: String):
  case None extends UsePriceMgmtAlgo("")
  case NotUse extends UsePriceMgmtAlgo("0")
  case Use extends UsePriceMgmtAlgo("1")

object Profile:
  enum Type:
    case NONE, Percents, Ratios, Shares

  final case class Allocation(account: String, amount: String)
end Profile

final case class Profile(
    name: String,
    ofType: Profile.Type,
    allocations: List[Profile.Allocation]
)

final case class TradeId(val id: String) {
  def full: String = id
  def key: String = id.split("\\.").head
}

final case class Bar(
    count: Int,
    time: String,
    open: Double,
    close: Double,
    high: Double,
    low: Double,
    wap: Decimal,
    volume: Decimal
)

final case class Alias(account: String, alias: String)

enum ScanCode:
  case TOP_PERC_GAIN,
    TOP_PERC_LOSE,
    MOST_ACTIVE,
    ALL_SYMBOLS_ASC,
    ALL_SYMBOLS_DESC,
    BOND_CUSIP_AZ,
    BOND_CUSIP_ZA,
    FAR_MATURITY_DATE,
    HALTED,
    HIGH_BOND_ASK_CURRENT_YIELD_ALL,
    HIGH_BOND_ASK_YIELD_ALL,
    HIGH_BOND_DEBT_2_BOOK_RATIO,
    HIGH_BOND_DEBT_2_EQUITY_RATIO,
    HIGH_BOND_DEBT_2_TAN_BOOK_RATIO,
    HIGH_BOND_EQUITY_2_BOOK_RATIO,
    HIGH_BOND_EQUITY_2_TAN_BOOK_RATIO,
    HIGH_BOND_NET_ASK_CURRENT_YIELD_ALL,
    HIGH_BOND_NET_ASK_YIELD_ALL,
    HIGH_BOND_NET_SPREAD_ALL,
    HIGH_BOND_SPREAD_ALL,
    HIGH_COUPON_RATE,
    HIGH_DIVIDEND_YIELD,
    HIGH_DIVIDEND_YIELD_IB,
    HIGHEST_SLB_BID,
    HIGH_GROWTH_RATE,
    HIGH_MOODY_RATING_ALL,
    HIGH_OPEN_GAP,
    HIGH_OPT_IMP_VOLAT,
    HIGH_OPT_IMP_VOLAT_OVER_HIST,
    HIGH_OPT_OPEN_INTEREST_PUT_CALL_RATIO,
    HIGH_OPT_VOLUME_PUT_CALL_RATIO,
    HIGH_PE_RATIO,
    HIGH_PRICE_2_BOOK_RATIO,
    HIGH_PRICE_2_TAN_BOOK_RATIO,
    HIGH_QUICK_RATIO,
    HIGH_RETURN_ON_EQUITY,
    HIGH_SYNTH_BID_REV_NAT_YIELD,
    HIGH_VS_13W_HL,
    HIGH_VS_26W_HL,
    HIGH_VS_52W_HL,
    HOT_BY_OPT_VOLUME,
    HOT_BY_PRICE,
    HOT_BY_PRICE_RANGE,
    HOT_BY_VOLUME,
    LIMIT_UP_DOWN,
    LOW_BOND_BID_CURRENT_YIELD_ALL,
    LOW_BOND_BID_YIELD_ALL,
    LOW_BOND_DEBT_2_BOOK_RATIO,
    LOW_BOND_DEBT_2_EQUITY_RATIO,
    LOW_BOND_DEBT_2_TAN_BOOK_RATIO,
    LOW_BOND_EQUITY_2_BOOK_RATIO,
    LOW_BOND_EQUITY_2_TAN_BOOK_RATIO,
    LOW_BOND_NET_BID_CURRENT_YIELD_ALL,
    LOW_BOND_NET_BID_YIELD_ALL,
    LOW_BOND_NET_SPREAD_ALL,
    LOW_BOND_SPREAD_ALL,
    LOW_COUPON_RATE,
    LOWEST_SLB_ASK,
    LOW_GROWTH_RATE,
    LOW_MOODY_RATING_ALL,
    LOW_OPEN_GAP,
    LOW_OPT_IMP_VOLAT,
    LOW_OPT_IMP_VOLAT_OVER_HIST,
    LOW_OPT_OPEN_INTEREST_PUT_CALL_RATIO,
    LOW_OPT_VOLUME_PUT_CALL_RATIO,
    LOW_PE_RATIO,
    LOW_PRICE_2_BOOK_RATIO,
    LOW_PRICE_2_TAN_BOOK_RATIO,
    LOW_QUICK_RATIO,
    LOW_RETURN_ON_EQUITY,
    LOW_SYNTH_ASK_REV_NAT_YIELD,
    LOW_VS_13W_HL,
    LOW_VS_26W_HL,
    LOW_VS_52W_HL,
    LOW_WAR_REL_IMP_VOLAT,
    MARKET_CAP_USD_ASC,
    MARKET_CAP_USD_DESC,
    MOST_ACTIVE_AVG_USD,
    MOST_ACTIVE_USD,
    NEAR_MATURITY_DATE,
    NOT_OPEN,
    OPT_OPEN_INTEREST_MOST_ACTIVE,
    OPT_VOLUME_MOST_ACTIVE,
    PMONITOR_AVAIL_CONTRACTS,
    PMONITOR_CTT,
    PMONITOR_IBOND,
    PMONITOR_RFQ,
    TOP_OPEN_PERC_GAIN,
    TOP_OPEN_PERC_LOSE,
    TOP_OPT_IMP_VOLAT_GAIN,
    TOP_OPT_IMP_VOLAT_LOSE,
    TOP_PRICE_RANGE,
    TOP_STOCK_BUY_IMBALANCE_ADV_RATIO,
    TOP_STOCK_SELL_IMBALANCE_ADV_RATIO,
    TOP_TRADE_COUNT,
    TOP_TRADE_RATE,
    TOP_VOLUME_RATE,
    WSH_NEXT_ANALYST_MEETING,
    WSH_NEXT_EARNINGS,
    WSH_NEXT_EVENT,
    WSH_NEXT_MAJOR_EVENT,
    WSH_PREV_ANALYST_MEETING,
    WSH_PREV_EARNINGS,
    WSH_PREV_EVENT
end ScanCode

enum MarketValueTag:
  case NetLiquidationByCurrency,
    CashBalance,
    TotalCashBalance,
    AccruedCash,
    StockMarketValue,
    OptionMarketValue,
    FutureOptionValue,
    FuturesPNL,
    UnrealizedPnL,
    RealizedPnL,
    ExchangeRate,
    FundValue,
    NetDividend,
    MutualFundValue,
    MoneyMarketFundValue,
    CorporateBondValue,
    TBondValue,
    TBillValue,
    WarrantValue,
    FxCashBalance

end MarketValueTag

enum Instrument:
  case STK,
    BOND,
    EFP,
    FUT_EU,
    FUT_HK,
    FUT_NA,
    FUT_US,
    IND_EU,
    IND_HK,
    IND_US,
    PMONITOR,
    PMONITORM,
    SLB_US,
    STOCK_EU,
    STOCK_HK,
    STOCK_NA,
    WAR_EU
  // override def toString: String = super.toString.replace('_', '.')
end Instrument

enum AccountSummaryTag:
  case AccountType, // balances
    NetLiquidation,
    TotalCashValue, // Total cash including futures pnl
    SettledCash, // For cash accounts, this is the same as TotalCashValue
    AccruedCash, // Net accrued interest
    BuyingPower, // The maximum amount of marginable US stocks the account can buy
    EquityWithLoanValue, // Cash + stocks + bonds + mutual funds
    PreviousEquityWithLoanValue,
    GrossPositionValue, // The sum of the absolute value of all stock and equity option positions
    RegTEquity,
    RegTMargin,
    SMA, // Special Memorandum Account

    // current margin
    InitMarginReq,
    MaintMarginReq,
    AvailableFunds,
    ExcessLiquidity,
    Cushion, // Excess liquidity as a percentage of net liquidation value

    // overnight margin
    FullInitMarginReq,
    FullMaintMarginReq,
    FullAvailableFunds,
    FullExcessLiquidity,

    // look-ahead margin
    LookAheadNextChange, // Time when look-ahead values take effect
    LookAheadInitMarginReq,
    LookAheadMaintMarginReq,
    LookAheadAvailableFunds,
    LookAheadExcessLiquidity,

    // misc
    HighestSeverity, // A measure of how close the account is to liquidation
    DayTradesRemaining, // The Number of Open/Close trades one could do before Pattern Day Trading is detected, a value of "-1" means user can do unlimited day trades.
    Leverage // GrossPositionValue / NetLiquidation
end AccountSummaryTag

final case class Group(
    name: String,
    defaultMethod: Method,
    account: List[String]
)
final case class Position(
    contract: Contract,
    account: String,
    position: Decimal,
    marketPrice: Double,
    marketValue: Double,
    averageCost: Double,
    unrealPnl: Double,
    realPnl: Double
)

enum OrderConditionType(val value: Int):
  case Unknown extends OrderConditionType(-1)
  case Price extends OrderConditionType(1)
  case Time extends OrderConditionType(3)
  case Margin extends OrderConditionType(4)
  case Execution extends OrderConditionType(5)
  case Volume extends OrderConditionType(6)
  case PercentChange extends OrderConditionType(7)

sealed trait OrderCondition:
  def conditionType: OrderConditionType
  def isConjunctionConnection: Boolean

case object UnknownOrderCondition extends OrderCondition:
  val conditionType = OrderConditionType.Unknown
  val isConjunctionConnection = false

sealed trait OperatorCondition extends OrderCondition:
  def isMore: Boolean

sealed trait ContractCondition extends OperatorCondition:
  def conId: Int
  def exchange: String

final case class ExecutionCondition(
    isConjunctionConnection: Boolean,
    secType: SecType,
    exchange: String,
    symbol: String
) extends OrderCondition {
  final val conditionType = OrderConditionType.Execution
}

final case class MarginCondition(
    isConjunctionConnection: Boolean,
    isMore: Boolean,
    percent: Int
) extends OrderCondition {
  final val conditionType = OrderConditionType.Margin
}

final case class PriceCondition(
    isConjunctionConnection: Boolean,
    isMore: Boolean,
    price: Double,
    conId: Int,
    exchange: String,
    triggerMethod: TriggerMethod
) extends ContractCondition {
  final val conditionType = OrderConditionType.Price
}

final case class VolumeCondition(
    isConjunctionConnection: Boolean,
    isMore: Boolean,
    volume: Int,
    conId: Int,
    exchange: String
) extends ContractCondition {
  final val conditionType = OrderConditionType.Volume
}

final case class PercentChangeCondition(
    isConjunctionConnection: Boolean,
    isMore: Boolean,
    changePercent: Double,
    conId: Int,
    exchange: String
) extends ContractCondition {
  final val conditionType = OrderConditionType.PercentChange
}

final case class TimeCondition(
    isConjunctionConnection: Boolean,
    isMore: Boolean,
    time: String
) extends OperatorCondition {
  final val conditionType = OrderConditionType.Time
}

final case class WshEventData(
    filter: String,
    fillWatchlist: Boolean,
    fillPortfolio: Boolean,
    fillCompetitors: Boolean,
    startDate: String,
    endDate: String,
    totalLimit: Int
)
