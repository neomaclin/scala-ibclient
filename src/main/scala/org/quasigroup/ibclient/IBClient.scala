package org.quasigroup.ibclient

import org.quasigroup.ibclient.response.ResponseMsg
import org.quasigroup.ibclient.response.ResponseMsg.*

import fs2.Stream

trait IBClient[F[_]]:

  def reqCurrentTime: F[CurrentTime]

  def reqFamilyCodes: F[FamilyCodes]

  def reqScannerParameters: F[ScannerParameters]

  // def reqOpenOrders(): F[]

  // def reqNewsBulletins(allMsgs: Boolean): F[UpdateNewsBulletin]

  def setServerLogLevel(level: Int): F[Unit]

  def reqPositions: Stream[F, PositionMsg]

  def cancelPositions: F[Unit]

  def reqManagedAccts: F[ManagedAccounts]

  def requestFA(faDataType: Int): F[ReceiveFA]

end IBClient

object IBClient:

  opaque type ServerVersion = Int

  extension (version: ServerVersion)
    def >=(otherVersion: ServerVersion): Boolean = version >= otherVersion
    def ==(otherVersion: ServerVersion): Boolean = version == otherVersion
    def <(otherVersion: ServerVersion): Boolean = version < otherVersion

  object ServerVersion:
    def apply(value: Int): ServerVersion = value

  val MIN_SERVER_VER_REAL_TIME_BARS: ServerVersion = 34
  val MIN_SERVER_VER_SCALE_ORDERS: ServerVersion = 35
  val MIN_SERVER_VER_SNAPSHOT_MKT_DATA: ServerVersion = 35
  val MIN_SERVER_VER_SSHORT_COMBO_LEGS: ServerVersion = 35
  val MIN_SERVER_VER_WHAT_IF_ORDERS: ServerVersion = 36
  val MIN_SERVER_VER_CONTRACT_CONID: ServerVersion = 37
  val MIN_SERVER_VER_PTA_ORDERS: ServerVersion = 39
  val MIN_SERVER_VER_FUNDAMENTAL_DATA: ServerVersion = 40
  val MIN_SERVER_VER_DELTA_NEUTRAL: ServerVersion = 40
  val MIN_SERVER_VER_CONTRACT_DATA_CHAIN: ServerVersion = 40
  val MIN_SERVER_VER_SCALE_ORDERS2: ServerVersion = 40
  val MIN_SERVER_VER_ALGO_ORDERS: ServerVersion = 41
  val MIN_SERVER_VER_EXECUTION_DATA_CHAIN: ServerVersion = 42
  val MIN_SERVER_VER_NOT_HELD: ServerVersion = 44
  val MIN_SERVER_VER_SEC_ID_TYPE: ServerVersion = 45
  val MIN_SERVER_VER_PLACE_ORDER_CONID: ServerVersion = 46
  val MIN_SERVER_VER_REQ_MKT_DATA_CONID: ServerVersion = 47
  val MIN_SERVER_VER_REQ_CALC_IMPLIED_VOLAT: ServerVersion = 49
  val MIN_SERVER_VER_REQ_CALC_OPTION_PRICE: ServerVersion = 50
  val MIN_SERVER_VER_CANCEL_CALC_IMPLIED_VOLAT: ServerVersion = 50
  val MIN_SERVER_VER_CANCEL_CALC_OPTION_PRICE: ServerVersion = 50
  val MIN_SERVER_VER_SSHORTX_OLD: ServerVersion = 51
  val MIN_SERVER_VER_SSHORTX: ServerVersion = 52
  val MIN_SERVER_VER_REQ_GLOBAL_CANCEL: ServerVersion = 53
  val MIN_SERVER_VER_HEDGE_ORDERS: ServerVersion = 54
  val MIN_SERVER_VER_REQ_MARKET_DATA_TYPE: ServerVersion = 55
  val MIN_SERVER_VER_OPT_OUT_SMART_ROUTING: ServerVersion = 56
  val MIN_SERVER_VER_SMART_COMBO_ROUTING_PARAMS: ServerVersion = 57
  val MIN_SERVER_VER_DELTA_NEUTRAL_CONID: ServerVersion = 58
  val MIN_SERVER_VER_SCALE_ORDERS3: ServerVersion = 60
  val MIN_SERVER_VER_ORDER_COMBO_LEGS_PRICE: ServerVersion = 61
  val MIN_SERVER_VER_TRAILING_PERCENT: ServerVersion = 62
  val MIN_SERVER_VER_DELTA_NEUTRAL_OPEN_CLOSE: ServerVersion = 66
  val MIN_SERVER_VER_ACCT_SUMMARY: ServerVersion = 67
  val MIN_SERVER_VER_TRADING_CLASS: ServerVersion = 68
  val MIN_SERVER_VER_SCALE_TABLE: ServerVersion = 69
  val MIN_SERVER_VER_LINKING: ServerVersion = 70
  val MIN_SERVER_VER_ALGO_ID: ServerVersion = 71
  val MIN_SERVER_VER_OPTIONAL_CAPABILITIES: ServerVersion = 72
  val MIN_SERVER_VER_ORDER_SOLICITED: ServerVersion = 73
  val MIN_SERVER_VER_LINKING_AUTH: ServerVersion = 74
  val MIN_SERVER_VER_PRIMARYEXCH: ServerVersion = 75
  val MIN_SERVER_VER_RANDOMIZE_SIZE_AND_PRICE: ServerVersion = 76
  val MIN_SERVER_VER_FRACTIONAL_POSITIONS: ServerVersion = 101
  val MIN_SERVER_VER_PEGGED_TO_BENCHMARK: ServerVersion = 102
  val MIN_SERVER_VER_MODELS_SUPPORT: ServerVersion = 103
  val MIN_SERVER_VER_SEC_DEF_OPT_PARAMS_REQ: ServerVersion = 104
  val MIN_SERVER_VER_EXT_OPERATOR: ServerVersion = 105
  val MIN_SERVER_VER_SOFT_DOLLAR_TIER: ServerVersion = 106
  val MIN_SERVER_VER_REQ_FAMILY_CODES: ServerVersion = 107
  val MIN_SERVER_VER_REQ_MATCHING_SYMBOLS: ServerVersion = 108
  val MIN_SERVER_VER_PAST_LIMIT: ServerVersion = 109
  val MIN_SERVER_VER_MD_SIZE_MULTIPLIER: ServerVersion = 110
  val MIN_SERVER_VER_CASH_QTY: ServerVersion = 111
  val MIN_SERVER_VER_REQ_MKT_DEPTH_EXCHANGES: ServerVersion = 112
  val MIN_SERVER_VER_TICK_NEWS: ServerVersion = 113
  val MIN_SERVER_VER_REQ_SMART_COMPONENTS: ServerVersion = 114
  val MIN_SERVER_VER_REQ_NEWS_PROVIDERS: ServerVersion = 115
  val MIN_SERVER_VER_REQ_NEWS_ARTICLE: ServerVersion = 116
  val MIN_SERVER_VER_REQ_HISTORICAL_NEWS: ServerVersion = 117
  val MIN_SERVER_VER_REQ_HEAD_TIMESTAMP: ServerVersion = 118
  val MIN_SERVER_VER_REQ_HISTOGRAM: ServerVersion = 119
  val MIN_SERVER_VER_SERVICE_DATA_TYPE: ServerVersion = 120
  val MIN_SERVER_VER_AGG_GROUP: ServerVersion = 121
  val MIN_SERVER_VER_UNDERLYING_INFO: ServerVersion = 122
  val MIN_SERVER_VER_CANCEL_HEADTIMESTAMP: ServerVersion = 123
  val MIN_SERVER_VER_SYNT_REALTIME_BARS: ServerVersion = 124
  val MIN_SERVER_VER_CFD_REROUTE: ServerVersion = 125
  val MIN_SERVER_VER_MARKET_RULES: ServerVersion = 126
  val MIN_SERVER_VER_PNL: ServerVersion = 127
  val MIN_SERVER_VER_NEWS_QUERY_ORIGINS: ServerVersion = 128
  val MIN_SERVER_VER_UNREALIZED_PNL: ServerVersion = 129
  val MIN_SERVER_VER_HISTORICAL_TICKS: ServerVersion = 130
  val MIN_SERVER_VER_MARKET_CAP_PRICE: ServerVersion = 131
  val MIN_SERVER_VER_PRE_OPEN_BID_ASK: ServerVersion = 132
  val MIN_SERVER_VER_REAL_EXPIRATION_DATE: ServerVersion = 134
  val MIN_SERVER_VER_REALIZED_PNL: ServerVersion = 135
  val MIN_SERVER_VER_LAST_LIQUIDITY: ServerVersion = 136
  val MIN_SERVER_VER_TICK_BY_TICK: ServerVersion = 137
  val MIN_SERVER_VER_DECISION_MAKER: ServerVersion = 138
  val MIN_SERVER_VER_MIFID_EXECUTION: ServerVersion = 139
  val MIN_SERVER_VER_TICK_BY_TICK_IGNORE_SIZE: ServerVersion = 140
  val MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE: ServerVersion = 141
  val MIN_SERVER_VER_WHAT_IF_EXT_FIELDS: ServerVersion = 142
  val MIN_SERVER_VER_SCANNER_GENERIC_OPTS: ServerVersion = 143
  val MIN_SERVER_VER_API_BIND_ORDER: ServerVersion = 144
  val MIN_SERVER_VER_ORDER_CONTAINER: ServerVersion = 145
  val MIN_SERVER_VER_SMART_DEPTH: ServerVersion = 146
  val MIN_SERVER_VER_REMOVE_NULL_ALL_CASTING: ServerVersion = 147
  val MIN_SERVER_VER_D_PEG_ORDERS: ServerVersion = 148
  val MIN_SERVER_VER_MKT_DEPTH_PRIM_EXCHANGE: ServerVersion = 149
  val MIN_SERVER_VER_REQ_COMPLETED_ORDERS: ServerVersion = 150
  val MIN_SERVER_VER_PRICE_MGMT_ALGO: ServerVersion = 151
  val MIN_SERVER_VER_STOCK_TYPE: ServerVersion = 152
  val MIN_SERVER_VER_ENCODE_MSG_ASCII7: ServerVersion = 153
  val MIN_SERVER_VER_SEND_ALL_FAMILY_CODES: ServerVersion = 154
  val MIN_SERVER_VER_NO_DEFAULT_OPEN_CLOSE: ServerVersion = 155
  val MIN_SERVER_VER_PRICE_BASED_VOLATILITY: ServerVersion = 156
  val MIN_SERVER_VER_REPLACE_FA_END: ServerVersion = 157
  val MIN_SERVER_VER_DURATION: ServerVersion = 158
  val MIN_SERVER_VER_MARKET_DATA_IN_SHARES: ServerVersion = 159
  val MIN_SERVER_VER_POST_TO_ATS: ServerVersion = 160
  val MIN_SERVER_VER_WSHE_CALENDAR: ServerVersion = 161
  val MIN_SERVER_VER_AUTO_CANCEL_PARENT: ServerVersion = 162
  val MIN_SERVER_VER_FRACTIONAL_SIZE_SUPPORT: ServerVersion = 163
  val MIN_SERVER_VER_SIZE_RULES: ServerVersion = 164
  val MIN_SERVER_VER_HISTORICAL_SCHEDULE: ServerVersion = 165
  val MIN_SERVER_VER_ADVANCED_ORDER_REJECT: ServerVersion = 166
  val MIN_SERVER_VER_USER_INFO: ServerVersion = 167
  val MIN_SERVER_VER_CRYPTO_AGGREGATED_TRADES: ServerVersion = 168
  val MIN_SERVER_VER_MANUAL_ORDER_TIME: ServerVersion = 169
  val MIN_SERVER_VER_PEGBEST_PEGMID_OFFSETS: ServerVersion = 170
  val MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS: ServerVersion = 171
  val MIN_SERVER_VER_IPO_PRICES: ServerVersion = 172
  val MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS_DATE: ServerVersion = 173
  val MIN_SERVER_VER_INSTRUMENT_TIMEZONE: ServerVersion = 174
  val MIN_SERVER_VER_HMDS_MARKET_DATA_IN_SHARES: ServerVersion = 175
  val MIN_SERVER_VER_BOND_ISSUERID: ServerVersion = 176
  val MIN_SERVER_VER_FA_PROFILE_DESUPPORT: ServerVersion = 177

  val MIN_VERSION: ServerVersion = 100
  // envelope encoding, applicable to useV100Plus mode only
  val MAX_VERSION: ServerVersion = MIN_SERVER_VER_FA_PROFILE_DESUPPORT // ditto

  def buildVersionString(
      minVersion: ServerVersion,
      maxVersion: ServerVersion
  ): String =
    "v" + (if minVersion < maxVersion then minVersion + ".." + maxVersion
           else minVersion)
