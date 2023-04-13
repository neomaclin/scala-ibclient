package org.quasigroup.ibclient.client

import cats.effect.std.Console
import cats.effect.{Async, Resource}
import com.comcast.ip4s.{Host, Port, SocketAddress}
import fs2.interop.scodec.StreamDecoder
import fs2.io.net.Network
import fs2.{Chunk, Stream}
import org.quasigroup.ibclient.client.response.ResponseMsg.*
import org.quasigroup.ibclient.client.types.ConnectionAck
import scodec.bits.ByteVector

trait IBClient[F[_]] {

  def eConnect(clientId: Int): F[ConnectionAck]

  def eDisconnect(): F[Unit]

  def reqCurrentTime(): F[CurrentTime]

  def reqFamilyCodes(): F[FamilyCodes]

  def reqScannerParameters(): F[ScannerParameters]

  // def reqOpenOrders(): F[]

  // def reqNewsBulletins(allMsgs: Boolean): F[UpdateNewsBulletin]

  def setServerLogLevel(level: Int): F[Unit]

  def reqPositions():  Stream[F,Position]

  def cancelPositions(): F[Unit]

  def reqManagedAccts(): F[ManagedAccounts]

  def requestFA(faDataType: Int): F[ReceiveFA]

}

object IBClient {
  val MIN_SERVER_VER_REAL_TIME_BARS = 34
  val MIN_SERVER_VER_SCALE_ORDERS = 35
  val MIN_SERVER_VER_SNAPSHOT_MKT_DATA = 35
  val MIN_SERVER_VER_SSHORT_COMBO_LEGS = 35
  val MIN_SERVER_VER_WHAT_IF_ORDERS = 36
  val MIN_SERVER_VER_CONTRACT_CONID = 37
  val MIN_SERVER_VER_PTA_ORDERS = 39
  val MIN_SERVER_VER_FUNDAMENTAL_DATA = 40
  val MIN_SERVER_VER_DELTA_NEUTRAL = 40
  val MIN_SERVER_VER_CONTRACT_DATA_CHAIN = 40
  val MIN_SERVER_VER_SCALE_ORDERS2 = 40
  val MIN_SERVER_VER_ALGO_ORDERS = 41
  val MIN_SERVER_VER_EXECUTION_DATA_CHAIN = 42
  val MIN_SERVER_VER_NOT_HELD = 44
  val MIN_SERVER_VER_SEC_ID_TYPE = 45
  val MIN_SERVER_VER_PLACE_ORDER_CONID = 46
  val MIN_SERVER_VER_REQ_MKT_DATA_CONID = 47
  val MIN_SERVER_VER_REQ_CALC_IMPLIED_VOLAT = 49
  val MIN_SERVER_VER_REQ_CALC_OPTION_PRICE = 50
  val MIN_SERVER_VER_CANCEL_CALC_IMPLIED_VOLAT = 50
  val MIN_SERVER_VER_CANCEL_CALC_OPTION_PRICE = 50
  val MIN_SERVER_VER_SSHORTX_OLD = 51
  val MIN_SERVER_VER_SSHORTX = 52
  val MIN_SERVER_VER_REQ_GLOBAL_CANCEL = 53
  val MIN_SERVER_VER_HEDGE_ORDERS = 54
  val MIN_SERVER_VER_REQ_MARKET_DATA_TYPE = 55
  val MIN_SERVER_VER_OPT_OUT_SMART_ROUTING = 56
  val MIN_SERVER_VER_SMART_COMBO_ROUTING_PARAMS = 57
  val MIN_SERVER_VER_DELTA_NEUTRAL_CONID = 58
  val MIN_SERVER_VER_SCALE_ORDERS3 = 60
  val MIN_SERVER_VER_ORDER_COMBO_LEGS_PRICE = 61
  val MIN_SERVER_VER_TRAILING_PERCENT = 62
  val MIN_SERVER_VER_DELTA_NEUTRAL_OPEN_CLOSE = 66
  val MIN_SERVER_VER_ACCT_SUMMARY = 67
  val MIN_SERVER_VER_TRADING_CLASS = 68
  val MIN_SERVER_VER_SCALE_TABLE = 69
  val MIN_SERVER_VER_LINKING = 70
  val MIN_SERVER_VER_ALGO_ID = 71
  val MIN_SERVER_VER_OPTIONAL_CAPABILITIES = 72
  val MIN_SERVER_VER_ORDER_SOLICITED = 73
  val MIN_SERVER_VER_LINKING_AUTH = 74
  val MIN_SERVER_VER_PRIMARYEXCH = 75
  val MIN_SERVER_VER_RANDOMIZE_SIZE_AND_PRICE = 76
  val MIN_SERVER_VER_FRACTIONAL_POSITIONS = 101
  val MIN_SERVER_VER_PEGGED_TO_BENCHMARK = 102
  val MIN_SERVER_VER_MODELS_SUPPORT = 103
  val MIN_SERVER_VER_SEC_DEF_OPT_PARAMS_REQ = 104
  val MIN_SERVER_VER_EXT_OPERATOR = 105
  val MIN_SERVER_VER_SOFT_DOLLAR_TIER = 106
  val MIN_SERVER_VER_REQ_FAMILY_CODES = 107
  val MIN_SERVER_VER_REQ_MATCHING_SYMBOLS = 108
  val MIN_SERVER_VER_PAST_LIMIT = 109
  val MIN_SERVER_VER_MD_SIZE_MULTIPLIER = 110
  val MIN_SERVER_VER_CASH_QTY = 111
  val MIN_SERVER_VER_REQ_MKT_DEPTH_EXCHANGES = 112
  val MIN_SERVER_VER_TICK_NEWS = 113
  val MIN_SERVER_VER_REQ_SMART_COMPONENTS = 114
  val MIN_SERVER_VER_REQ_NEWS_PROVIDERS = 115
  val MIN_SERVER_VER_REQ_NEWS_ARTICLE = 116
  val MIN_SERVER_VER_REQ_HISTORICAL_NEWS = 117
  val MIN_SERVER_VER_REQ_HEAD_TIMESTAMP = 118
  val MIN_SERVER_VER_REQ_HISTOGRAM = 119
  val MIN_SERVER_VER_SERVICE_DATA_TYPE = 120
  val MIN_SERVER_VER_AGG_GROUP = 121
  val MIN_SERVER_VER_UNDERLYING_INFO = 122
  val MIN_SERVER_VER_CANCEL_HEADTIMESTAMP = 123
  val MIN_SERVER_VER_SYNT_REALTIME_BARS = 124
  val MIN_SERVER_VER_CFD_REROUTE = 125
  val MIN_SERVER_VER_MARKET_RULES = 126
  val MIN_SERVER_VER_PNL = 127
  val MIN_SERVER_VER_NEWS_QUERY_ORIGINS = 128
  val MIN_SERVER_VER_UNREALIZED_PNL = 129
  val MIN_SERVER_VER_HISTORICAL_TICKS = 130
  val MIN_SERVER_VER_MARKET_CAP_PRICE = 131
  val MIN_SERVER_VER_PRE_OPEN_BID_ASK = 132
  val MIN_SERVER_VER_REAL_EXPIRATION_DATE = 134
  val MIN_SERVER_VER_REALIZED_PNL = 135
  val MIN_SERVER_VER_LAST_LIQUIDITY = 136
  val MIN_SERVER_VER_TICK_BY_TICK = 137
  val MIN_SERVER_VER_DECISION_MAKER = 138
  val MIN_SERVER_VER_MIFID_EXECUTION = 139
  val MIN_SERVER_VER_TICK_BY_TICK_IGNORE_SIZE = 140
  val MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE = 141
  val MIN_SERVER_VER_WHAT_IF_EXT_FIELDS = 142
  val MIN_SERVER_VER_SCANNER_GENERIC_OPTS = 143
  val MIN_SERVER_VER_API_BIND_ORDER = 144
  val MIN_SERVER_VER_ORDER_CONTAINER = 145
  val MIN_SERVER_VER_SMART_DEPTH = 146
  val MIN_SERVER_VER_REMOVE_NULL_ALL_CASTING = 147
  val MIN_SERVER_VER_D_PEG_ORDERS = 148
  val MIN_SERVER_VER_MKT_DEPTH_PRIM_EXCHANGE = 149
  val MIN_SERVER_VER_REQ_COMPLETED_ORDERS = 150
  val MIN_SERVER_VER_PRICE_MGMT_ALGO = 151
  val MIN_SERVER_VER_STOCK_TYPE = 152
  val MIN_SERVER_VER_ENCODE_MSG_ASCII7 = 153
  val MIN_SERVER_VER_SEND_ALL_FAMILY_CODES = 154
  val MIN_SERVER_VER_NO_DEFAULT_OPEN_CLOSE = 155
  val MIN_SERVER_VER_PRICE_BASED_VOLATILITY = 156
  val MIN_SERVER_VER_REPLACE_FA_END = 157
  val MIN_SERVER_VER_DURATION = 158
  val MIN_SERVER_VER_MARKET_DATA_IN_SHARES = 159
  val MIN_SERVER_VER_POST_TO_ATS = 160
  val MIN_SERVER_VER_WSHE_CALENDAR = 161
  val MIN_SERVER_VER_AUTO_CANCEL_PARENT = 162
  val MIN_SERVER_VER_FRACTIONAL_SIZE_SUPPORT = 163
  val MIN_SERVER_VER_SIZE_RULES = 164
  val MIN_SERVER_VER_HISTORICAL_SCHEDULE = 165
  val MIN_SERVER_VER_ADVANCED_ORDER_REJECT = 166
  val MIN_SERVER_VER_USER_INFO = 167
  val MIN_SERVER_VER_CRYPTO_AGGREGATED_TRADES = 168
  val MIN_SERVER_VER_MANUAL_ORDER_TIME = 169
  val MIN_SERVER_VER_PEGBEST_PEGMID_OFFSETS = 170
  val MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS = 171
  val MIN_SERVER_VER_IPO_PRICES = 172
  val MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS_DATE = 173
  val MIN_SERVER_VER_INSTRUMENT_TIMEZONE = 174
  val MIN_SERVER_VER_HMDS_MARKET_DATA_IN_SHARES = 175
  val MIN_SERVER_VER_BOND_ISSUERID = 176
  val MIN_VERSION = 100
  // envelope encoding, applicable to useV100Plus mode only
  val MAX_VERSION = MIN_SERVER_VER_BOND_ISSUERID // ditto

  def buildVersionString(minVersion: Int, maxVersion: Int): String =
    "v" + (if minVersion < maxVersion then minVersion + ".." + maxVersion
           else minVersion)

}
