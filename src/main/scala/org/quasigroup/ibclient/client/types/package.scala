package org.quasigroup.ibclient.client.types

import cats.data.State

import scala.math.BigDecimal

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
    openClose: Int,
    shortSaleSlot: Int,
    designatedLocation: String,
    exemptCode: Int
)

final case class Contract(
    conId: Int,
    symbol: String,
    secType: SecType,
    lastTradeDateOrContractMonth: String,
    strike: Double,
    right: ContractRight,
    multiplier: String,
    exchange: String,
    currency: String,
    localSymbol: String,
    tradingClass: String,
    comboLegs: List[ComboLeg] = Nil,
    primaryExch: String = "",
    includeExpired: Boolean = false,
    secIdType: SecIdType = SecIdType.Ignored,
    secId: String = "",
    issuerId: String = "",
    deltaNeutralContract: Option[DeltaNeutralContract] = None
)

final case class ContractDetails(
    contract: Contract,
    marketName: String,
    minTick: Double,
    orderTypes: String,
    validExchanges: String,
    underConId: Int,
    longName: String,
    contractMonth: String,
    industry: String,
    category: String,
    subcategory: String,
    timeZoneId: String,
    tradingHours: String,
    liquidHours: String,
    evRule: String,
    evMultiplier: Double
)
final case class ContractDescription(
    contract: Contract,
    derivativeSecTypes: List[String]
)
final case class DeltaNeutralContract(conid: Int, delta: Double, price: Double)

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
  case StartTime, EndTime, AllowPastEndTime, MaxPctVol, PctVol, StrategyType,
    NoTakeLiq, RiskAversion, ForceCompletion, DisplaySize, GetDone,
    NoTradeAhead, UseOddLots,
    ComponentSize, TimeBetweenOrders, RandomizeTime20, RandomizeSize55,
    GiveUp, CatchUp, WaitForFill
end AlgoParam

enum AlgoStrategy(val params: List[AlgoParam]):
  case Ignored extends AlgoStrategy(Nil)
  case Vwap
      extends AlgoStrategy(
        List(
          AlgoParam.StartTime,
          AlgoParam.EndTime,
          AlgoParam.MaxPctVol,
          AlgoParam.NoTakeLiq,
          AlgoParam.GetDone,
          AlgoParam.NoTradeAhead,
          AlgoParam.UseOddLots
        )
      )
  case Twap
      extends AlgoStrategy(
        List(
          AlgoParam.StartTime,
          AlgoParam.EndTime,
          AlgoParam.AllowPastEndTime,
          AlgoParam.StrategyType
        )
      )
  case ArrivalPx
      extends AlgoStrategy(
        List(
          AlgoParam.StartTime,
          AlgoParam.EndTime,
          AlgoParam.AllowPastEndTime,
          AlgoParam.MaxPctVol,
          AlgoParam.RiskAversion,
          AlgoParam.ForceCompletion
        )
      )
  case DarkIce
      extends AlgoStrategy(
        List(
          AlgoParam.StartTime,
          AlgoParam.EndTime,
          AlgoParam.AllowPastEndTime,
          AlgoParam.DisplaySize
        )
      )
  case PctVol
      extends AlgoStrategy(
        List(
          AlgoParam.StartTime,
          AlgoParam.EndTime,
          AlgoParam.PctVol,
          AlgoParam.NoTakeLiq
        )
      )
  case AD
      extends AlgoStrategy(
        List(
          AlgoParam.StartTime,
          AlgoParam.EndTime,
          AlgoParam.ComponentSize,
          AlgoParam.TimeBetweenOrders,
          AlgoParam.RandomizeTime20,
          AlgoParam.RandomizeSize55,
          AlgoParam.GiveUp,
          AlgoParam.CatchUp,
          AlgoParam.WaitForFill
        )
      )
end AlgoStrategy

enum HedgeType:
  case Ignored, Delta, Beta, Fx, Pair

enum ContractRight:
  case Ignored, Put, Call

object ContractRight:
  def fromString(str: String) =
    str.headOption
      .collect {
        case 'P' => Put
        case 'C' => Call
      }
      .getOrElse(Ignored)

enum VolatilityType:
  case Ignored, Daily, Annual

enum ReferencePriceType:
  case Ignored, Midpoint, BidOrAsk

enum TriggerMethod(val value: Int):
  case Default extends TriggerMethod(0)
  case DoubleBidAsk extends TriggerMethod(1)
  case Last extends TriggerMethod(2)
  case DoubleLast extends TriggerMethod(3)
  case BidAsk extends TriggerMethod(4)
  case LastOrBidAsk extends TriggerMethod(7)
  case Midpoint extends TriggerMethod(8)
end TriggerMethod

enum Action:
  case BUY, SELL, SSHORT

enum Rule80A(val value: String):
  case Ignored extends Rule80A("")
  case IndivArb extends Rule80A("J")
  case IndivBigNonArb extends Rule80A("K")
  case IndivSmallNonArb extends Rule80A("I")
  case INST_ARB extends Rule80A("U")
  case InstBigNonArb extends Rule80A("Y")
  case InstSmallNonArb extends Rule80A("A")
end Rule80A

enum OcaType:
  case Ignored, CancelWithBlocking, ReduceWithBlocking, ReduceWithoutBlocking

enum TimeInForce:
  case DAY, GTC, OPG, IOC, GTD, GTT, AUC, FOK, GTX, DTC

enum ExerciseType:
  case Ignored, Exercise, Lapse

enum FundamentalType:
  case ReportSnapshot, ReportsFinSummary, ReportRatios, ReportsFinStatements,
    RESC, CalendarReport

enum WhatToShow:
  case TRADES, MIDPOINT, BID,
    ASK, // << only these are valid for real-time bars
    BID_ASK, HISTORICAL_VOLATILITY, OPTION_IMPLIED_VOLATILITY, YIELD_ASK,
    YIELD_BID, YIELD_BID_ASK, YIELD_LAST
end WhatToShow

enum BarSize:
  case _1_secs, _5_secs, _10_secs, _15_secs, _30_secs, _1_min, _2_mins,
    _3_mins, _5_mins, _10_mins, _15_mins, _20_mins, _30_mins, _1_hour,
    _4_hours, _1_day, _1_week
end BarSize

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
  case Ignored, CUSIP, SEDOL, ISIN, RIC

enum SecType:
  case Ignored, STK, OPT, FUT, CASH, BOND, CFD, FOP, WAR, IOPT, FWD, BAG, IND,
    BILL, FUND, FIXED, SLB, NEWS, CMDTY, BSK, ICU, ICS, CRYPTO
end SecType

enum MktDataType:
  case Unknown, Realtime, Frozen, Delayed, DelayedFrozen

final case class DepthMktDataDescription(
    exchange: String,
    secType: SecType,
    listingExch: String,
    serviceDataType: String,
    aggGroup: Int
)
enum Method:
  case Ignored, EqualQuantity, AvailableEquity, NetLiq, PctChange

enum TickType(index: Int, field: String):
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
  case SHORT_TERM_VOLUME_3_MIN extends TickType(63, "shortTermVolume3Min")
  case SHORT_TERM_VOLUME_5_MIN extends TickType(64, "shortTermVolume5Min")
  case SHORT_TERM_VOLUME_10_MIN extends TickType(65, "shortTermVolume10Min")
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

  case UNKNOWN extends TickType(Integer.MAX_VALUE, "unknown")

end TickType

final case class CommissionReport(
    execId: String,
    commission: Double,
    currency: String,
    realizedPNL: Double,
    `yield`: Double,
    yieldRedemptionDat: Int
)

enum Liquidities:
  case Ignored, Added, Removed, RoudedOut

final case class Execution(
    orderId: Int,
    clientId: Int,
    execId: String,
    time: String,
    acctNumber: String,
    exchange: String,
    side: String,
    shares: Int,
    price: Double,
    permId: Int,
    liquidation: Liquidities,
    cumQty: Int,
    avgPrice: Double,
    orderRef: String,
    evRule: String,
    evMultiplier: Double
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
    scanCode: String,
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
    account: String,
    settlingFirm: String,
    clearingAccount: String,
    clearingIntent: String,
    action: Action,
    algoStrategy: AlgoStrategy,
    algoId: String,
    allOrIgnored: Boolean,
    auxPrice: Double,
    blockOrder: Boolean,
    clientId: Int,
    continuousUpdate: Int,
    delta: Double,
    deltaNeutralAuxPrice: Double,
    deltaNeutralConId: Int,
    deltaNeutralOpenClose: String,
    deltaNeutralShortSale: Boolean,
    deltaNeutralShortSaleSlot: Int,
    deltaNeutralDesignatedLocation: String,
    deltaNeutralOrderType: Order.Type,
    discretionaryAmt: Double,
    displaySize: Int,
    eTradeOnly: Boolean,
    faGroup: String,
    faMethod: Method,
    faPercentage: String,
    faProfile: String,
    firmQuoteOnly: Boolean,
    goodAfterTime: String,
    goodTillDate: String,
    hedgeParam: String,
    hedgeType: HedgeType,
    hidden: Boolean,
    lmtPrice: Double,
    minQty: Int,
    nbboPriceCap: Double,
    notHeld: Boolean,
    solicited: Boolean,
    ocaGroup: String,
    ocaType: OcaType,
    optOutSmartRouting: Boolean,
    orderId: Int,
    orderRef: String,
    orderType: Order.Type,
    outsideRth: Boolean,
    overridePercentageConstraints: Boolean,
    openClose: String,
    origin: Int,
    shortSaleSlot: Int,
    designatedLocation: String,
    exemptCode: Int,
    parentId: Int,
    percentOffset: Double,
    permId: Long,
    referencePriceType: ReferencePriceType,
    rule80A: Rule80A,
    scaleAutoReset: Boolean,
    scaleInitFillQty: Int,
    scaleInitLevelSize: Int,
    scaleInitPosition: Int,
    scalePriceAdjustInterval: Int,
    scalePriceAdjustValue: Double,
    scalePriceIncrement: Double,
    scaleProfitOffset: Double,
    scaleRandomPercent: Boolean,
    scaleSubsLevelSize: Int,
    startingPrice: Double,
    stockRangeLower: Double,
    stockRangeUpper: Double,
    stockRefPrice: Double,
    basisPoints: Double,
    basisPointsType: Int,
    sweepToFill: Boolean,
    tif: TimeInForce,
    totalQuantity: Int,
    trailingPercent: Double,
    trailStopPrice: Double,
    transmit: Boolean,
    triggerMethod: TriggerMethod,
    activeStartTime: String,
    activeStopTime: String,
    algoParams: List[TagValue],
    volatility: Double,
    volatilityType: VolatilityType,
    whatIf: Boolean,
    scaleTable: String,
    auctionStrategy: Int,
    orderComboLegs: List[Order.ComboLeg],
    deltaNeutralSettlingFirm: String,
    deltaNeutralClearingAccount: String,
    deltaNeutralClearingIntent: String,
    smartComboRoutingParams: List[TagValue],
    orderMiscOptions: List[TagValue]
)

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
  val EMPTY_STR = ""

  enum Type(val apiString: String):
    case Ignored extends Type("")
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
  end Type

  final case class ComboLeg(price: Double)

  final case class State(
      status: String,
      initMargin: String,
      maintMargin: String,
      equityWithLoan: String,
      commission: Double,
      minCommission: Double,
      maxCommission: Double,
      commissionCurrency: String,
      warningText: String
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

enum UsePriceMgmtAlgo:
  case Default, NotUse, Use
final case class ConnectionAck(serverVersion: Int, time: String)

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

final case class TradeId(private val id: String) {
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
    DayTradesRemaining, // The Number of Open/Close trades one could do before Pattern Day Trading is detected; a value of "-1" means user can do unlimited day trades.
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

enum OrderConditionType(private val value: Int):
  case Price extends OrderConditionType(1)
  case Time extends OrderConditionType(3)
  case Margin extends OrderConditionType(4)
  case Execution extends OrderConditionType(5)
  case Volume extends OrderConditionType(6)
  case PercentChange extends OrderConditionType(7)

final case class OrderCondition(
    conditionType: OrderConditionType,
    isConjunctionConnection: Boolean
)

final case class WshEventData(
    filter: String,
    fillWatchlist: Boolean,
    fillPortfolio: Boolean,
    fillCompetitors: Boolean,
    startDate: String,
    endDate: String,
    totalLimit: Int
)
