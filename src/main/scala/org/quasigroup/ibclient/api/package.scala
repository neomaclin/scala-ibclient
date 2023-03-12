package org.quasigroup.ibclient.api

import org.quasigroup.ibclient.client.types.Method

object Profile {
  enum Type {
    case NONE, Percents, Ratios, Shares
  }

  final case class Allocation(account: String, amount: String)

}

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
    time: Long,
    high: Double,
    low: Double,
    open: Double,
    close: Double,
    wap: Double,
    volume: Long,
    count: Int
)

final case class Alias(account: String, alias: String)

enum ScanCode {
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
}

enum MarketValueTag {
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
}

enum Instrument {
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
}

enum AccountSummaryTag {
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
}
final case class Group(
    name: String,
    defaultMethod: Method,
    account: List[String]
)
final case class Position(
    contract: Nothing,
    account: Nothing,
    position: Int,
    marketPrice: Double,
    marketValue: Double,
    averageCost: Double,
    unrealPnl: Double,
    realPnl: Double
)
