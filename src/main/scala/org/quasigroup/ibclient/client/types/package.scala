package org.quasigroup.ibclient.client.types

final case class ComboLeg(p_conId: Int, p_ratio: Int, p_action: Nothing, p_exchange: Nothing,
                          p_openClose: Int, p_shortSaleSlot: Int, p_designatedLocation: Nothing, p_exemptCode: Int)
final case class Contract(p_conId: Int, p_symbol: String, p_secType: String, p_expiry: String,
                          p_strike: Double, p_right: String, p_multiplier: String,
                          p_exchange: String, p_currency: String, p_localSymbol: String, p_tradingClass: String, p_comboLegs: List[ComboLeg], p_primaryExch: String, p_includeExpired: Boolean,
                          p_secIdType: String, p_secId: String )
final case class ContractDetails(p_contract: Contract, p_marketName: String, p_minTick: Double, p_orderTypes: String, p_validExchanges: String, p_underConId: Int, p_longName: String, p_contractMonth: String, p_industry: String, p_category: String, p_subcategory: String, p_timeZoneId: String, p_tradingHours: String, p_liquidHours: String, p_evRule: String, p_evMultiplier: Double)
final case class DeltaNeutralContract(conid: Int, delta: Double, price: Double)

object ComboLeg {
  enum OpenClose {
    case Same, Open, Close, Unknown
  }
}

enum ComboParam {
  case NonGuaranteed, PriceCondConid, CondPriceMax, CondPriceMin,
    ChangeToMktTime1, ChangeToMktTime2, DiscretionaryPct, DontLeginNext,
    LeginPrio, MaxSegSize
}

enum AlgoParam {
  case startTime, endTime, allowPastEndTime, maxPctVol, pctVol, strategyType,
    noTakeLiq, riskAversion, forceCompletion, displaySize, getDone,
    noTradeAhead, useOddLots,
    componentSize, timeBetweenOrders, randomizeTime20, randomizeSize55,
    giveUp, catchUp, waitForFill
}

enum HedgeType {
  case None, Delta, Beta, Fx, Pair
}

enum Right {
  case None, Put, Call
}

enum VolatilityType {
  case None, Daily, Annual
}

enum ReferencePriceType {
  case None, Midpoint, BidOrAsk
}

enum TriggerMethod(val value: Int) {
  case Default extends TriggerMethod(0)
  case DoubleBidAsk extends TriggerMethod(1)
  case Last extends TriggerMethod(2)
  case DoubleLast extends TriggerMethod(3)
  case BidAsk extends TriggerMethod(4)
  case LastOrBidAsk extends TriggerMethod(7)
  case Midpoint extends TriggerMethod(8)
}

enum Action {
  case BUY, SELL, SSHORT
}

enum Rule80A(val value: String) {
  case None extends Rule80A("")
  case IndivArb extends Rule80A("J")
  case IndivBigNonArb extends Rule80A("K")
  case IndivSmallNonArb extends Rule80A("I")
  case INST_ARB extends Rule80A("U")
  case InstBigNonArb extends Rule80A("Y")
  case InstSmallNonArb extends Rule80A("A")
}

enum OcaType {
  case None, CancelWithBlocking, ReduceWithBlocking, ReduceWithoutBlocking
}

enum TimeInForce {
  case DAY, GTC, OPG, IOC, GTD, GTT, AUC, FOK, GTX, DTC
}

enum ExerciseType {
  case None, Exercise, Lapse
}

enum FundamentalType {
  case ReportSnapshot, ReportsFinSummary, ReportRatios, ReportsFinStatements,
    RESC, CalendarReport
}

enum WhatToShow {
  case TRADES, MIDPOINT, BID,
    ASK, // << only these are valid for real-time bars
    BID_ASK, HISTORICAL_VOLATILITY, OPTION_IMPLIED_VOLATILITY, YIELD_ASK,
    YIELD_BID, YIELD_BID_ASK, YIELD_LAST
}

enum BarSize {
  case _1_secs, _5_secs, _10_secs, _15_secs, _30_secs, _1_min, _2_mins,
    _3_mins, _5_mins, _10_mins, _15_mins, _20_mins, _30_mins, _1_hour,
    _4_hours, _1_day, _1_week
}

enum DurationUnit {
  case SECOND, DAY, WEEK, MONTH, YEAR
}

enum DeepType {
  case INSERT, UPDATE, DELETE
}

enum DeepSide {
  case SELL, BUY
}

enum NewsType {
  case UNKNOWN, BBS, LIVE_EXCH, DEAD_EXCH, HTML, POPUP_TEXT, POPUP_HTML
}

enum FADataType {
  case UNUSED, GROUPS, PROFILES, ALIASES
}

enum SecIdType {
  case None, CUSIP, SEDOL, ISIN, RIC;

}

enum SecType {
  case None, STK, OPT, FUT, CASH, BOND, CFD, FOP, WAR, IOPT, FWD, BAG, IND,
    BILL, FUND, FIXED, SLB, NEWS, CMDTY, BSK, ICU, ICS

}

enum MktDataType {
  case Unknown, Realtime, Frozen

}

enum Method {
  case None, EqualQuantity, AvailableEquity, NetLiq, PctChange
}

enum TickType(index: Int, field: String) {
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
  case BID_EFP_COMPUTATION extends TickType(38, "bidEFP")
  case ASK_EFP_COMPUTATION extends TickType(39, "askEFP")
  case LAST_EFP_COMPUTATION extends TickType(40, "lastEFP")
  case OPEN_EFP_COMPUTATION extends TickType(41, "openEFP")
  case HIGH_EFP_COMPUTATION extends TickType(42, "highEFP")
  case LOW_EFP_COMPUTATION extends TickType(43, "lowEFP")
  case CLOSE_EFP_COMPUTATION extends TickType(44, "closeEFP")
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
  case UNKNOWN extends TickType(Integer.MAX_VALUE, "unknown");
}

enum MarketDataType {
  case RealTime, Frozen, Unknown
}

final case class CommissionReport(
          m_execId: String ,
      m_commission: Double,
m_currency: String,
 m_realizedPNL: Double,
 m_yield: Double,
pm_yieldRedemptionDat: Int
                                 )
final case class Execution(p_orderId: Int, p_clientId: Int, p_execId: String, p_time: String, p_acctNumber: String, p_exchange: String, p_side: String, p_shares: Int, p_price: Double, p_permId: Int, p_liquidation: Int, p_cumQty: Int, p_avgPrice: Double, p_orderRef: String, p_evRule: String, p_evMultiplier: Double)
final case class ExecutionFilter(p_clientId: Int, p_acctCode: String, p_time: String, p_symbol: String, p_secType: String, p_exchange: String, p_side: String)

opaque type ComboContract = Contract
opaque type FutContract = Contract
opaque type OptContract = Contract
opaque type StkContract = Contract


object Order {
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

  enum Type(val apiString: String) {
    case None  extends Type("")
    case MKT  extends Type("MKT")
    case LMT  extends Type("LMT")
    case STP  extends Type("STP")
    case STP_LMT  extends Type("STP LMT")
    case REL  extends Type("REL")
    case TRAIL  extends Type("TRAIL")
    case BOX_TOP  extends Type("BOX TOP")
    case FIX_PEGGED  extends Type("FIX PEGGED")
    case LIT  extends Type("LIT")
    case LMT_PLUS_MKT  extends Type("LMT + MKT")
    case LOC  extends Type("LOC")
    case MIT  extends Type("MIT")
    case MKT_PRT  extends Type("MKT PRT")
    case MOC  extends Type("MOC")
    case MTL  extends Type("MTL")
    case PASSV_REL  extends Type("PASSV REL")
    case PEG_BENCH  extends Type("PEG BENCH")
    case PEG_MID  extends Type("PEG MID")
    case PEG_MKT  extends Type("PEG MKT")
    case PEG_PRIM  extends Type("PEG PRIM")
    case PEG_STK  extends Type("PEG STK")
    case REL_PLUS_LMT  extends Type("REL + LMT")
    case REL_PLUS_MKT  extends Type("REL + MKT")
    case STP_PRT  extends Type("STP PRT")
    case TRAIL_LIMIT  extends Type("TRAIL LIMIT")
    case TRAIL_LIT  extends Type("TRAIL LIT")
    case TRAIL_LMT_PLUS_MKT  extends Type("TRAIL LMT + MKT")
    case TRAIL_MIT  extends Type("TRAIL MIT")
    case TRAIL_REL_PLUS_MKT  extends Type("TRAIL REL + MKT")
    case VOL  extends Type("VOL")
    case VWAP  extends Type("VWAP")
    case QUOTE  extends Type("QUOTE");

  }
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

  enum Status {

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
  }

}
