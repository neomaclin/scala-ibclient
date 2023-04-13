package org.quasigroup.ibclient.client.exceptions

object EClientErrors {
  final case class CodeMsgPair(code: Int, msg: String)

  val NO_VALID_ID: Int = -1
  val ALREADY_CONNECTED = EClientErrors.CodeMsgPair(501, "Already connected.")
  val CONNECT_FAIL = EClientErrors.CodeMsgPair(
    502,
    "Couldn't connect to TWS. Confirm that \"Enable ActiveX and Socket Clients\" " + "is enabled and connection port is the same as \"Socket Port\" on the TWS \"Edit->Global Configuration...->API->Settings\" menu. " + "Live Trading ports: TWS: 7496; IB Gateway: 4001. Simulated Trading ports for new installations of version 954.1 or newer: " + "TWS: 7497; IB Gateway: 4002"
  )
  val UPDATE_TWS = EClientErrors.CodeMsgPair(
    503,
    "The TWS is out of date and must be upgraded."
  )
  val NOT_CONNECTED = EClientErrors.CodeMsgPair(504, "Not connected")
  val UNKNOWN_ID =
    EClientErrors.CodeMsgPair(505, "Fatal Error: Unknown message id.")
  val UNSUPPORTED_VERSION =
    EClientErrors.CodeMsgPair(506, "Unsupported Version")
  val BAD_LENGTH = EClientErrors.CodeMsgPair(507, "Bad Message Length")
  val BAD_MESSAGE = EClientErrors.CodeMsgPair(508, "Bad Message")
  val FAIL_SEND = EClientErrors.CodeMsgPair(
    509,
    "Failed to send message - "
  ) // generic message; all future messages should use this

  val FAIL_SEND_REQMKT =
    EClientErrors.CodeMsgPair(510, "Request Market Data Sending Error - ")
  val FAIL_SEND_CANMKT =
    EClientErrors.CodeMsgPair(511, "Cancel Market Data Sending Error - ")
  val FAIL_SEND_ORDER = EClientErrors.CodeMsgPair(512, "Order Sending Error - ")
  val FAIL_SEND_ACCT =
    EClientErrors.CodeMsgPair(513, "Account Update Request Sending Error -")
  val FAIL_SEND_EXEC =
    EClientErrors.CodeMsgPair(514, "Request For Executions Sending Error -")
  val FAIL_SEND_CORDER =
    EClientErrors.CodeMsgPair(515, "Cancel Order Sending Error -")
  val FAIL_SEND_OORDER =
    EClientErrors.CodeMsgPair(516, "Request Open Order Sending Error -")
  val UNKNOWN_CONTRACT = EClientErrors.CodeMsgPair(
    517,
    "Unknown contract. Verify the contract details supplied."
  )
  val FAIL_SEND_REQCONTRACT =
    EClientErrors.CodeMsgPair(518, "Request Contract Data Sending Error - ")
  val FAIL_SEND_REQMKTDEPTH =
    EClientErrors.CodeMsgPair(519, "Request Market Depth Sending Error - ")
  val FAIL_SEND_CANMKTDEPTH =
    EClientErrors.CodeMsgPair(520, "Cancel Market Depth Sending Error - ")
  val FAIL_SEND_SERVER_LOG_LEVEL =
    EClientErrors.CodeMsgPair(521, "Set Server Log Level Sending Error - ")
  val FAIL_SEND_FA_REQUEST =
    EClientErrors.CodeMsgPair(522, "FA Information Request Sending Error - ")
  val FAIL_SEND_FA_REPLACE =
    EClientErrors.CodeMsgPair(523, "FA Information Replace Sending Error - ")
  val FAIL_SEND_REQSCANNER = EClientErrors.CodeMsgPair(
    524,
    "Request Scanner Subscription Sending Error - "
  )
  val FAIL_SEND_CANSCANNER = EClientErrors.CodeMsgPair(
    525,
    "Cancel Scanner Subscription Sending Error - "
  )
  val FAIL_SEND_REQSCANNERPARAMETERS =
    EClientErrors.CodeMsgPair(526, "Request Scanner Parameter Sending Error - ")
  val FAIL_SEND_REQHISTDATA =
    EClientErrors.CodeMsgPair(527, "Request Historical Data Sending Error - ")
  val FAIL_SEND_CANHISTDATA =
    EClientErrors.CodeMsgPair(528, "Request Historical Data Sending Error - ")
  val FAIL_SEND_REQRTBARS = EClientErrors.CodeMsgPair(
    529,
    "Request Real-time Bar Data Sending Error - "
  )
  val FAIL_SEND_CANRTBARS =
    EClientErrors.CodeMsgPair(530, "Cancel Real-time Bar Data Sending Error - ")
  val FAIL_SEND_REQCURRTIME =
    EClientErrors.CodeMsgPair(531, "Request Current Time Sending Error - ")
  val FAIL_SEND_REQFUNDDATA =
    EClientErrors.CodeMsgPair(532, "Request Fundamental Data Sending Error - ")
  val FAIL_SEND_CANFUNDDATA =
    EClientErrors.CodeMsgPair(533, "Cancel Fundamental Data Sending Error - ")
  val FAIL_SEND_REQCALCIMPLIEDVOLAT = EClientErrors.CodeMsgPair(
    534,
    "Request Calculate Implied Volatility Sending Error - "
  )
  val FAIL_SEND_REQCALCOPTIONPRICE = EClientErrors.CodeMsgPair(
    535,
    "Request Calculate Option Price Sending Error - "
  )
  val FAIL_SEND_CANCALCIMPLIEDVOLAT = EClientErrors.CodeMsgPair(
    536,
    "Cancel Calculate Implied Volatility Sending Error - "
  )
  val FAIL_SEND_CANCALCOPTIONPRICE = EClientErrors.CodeMsgPair(
    537,
    "Cancel Calculate Option Price Sending Error - "
  )
  val FAIL_SEND_REQGLOBALCANCEL =
    EClientErrors.CodeMsgPair(538, "Request Global Cancel Sending Error - ")
  val FAIL_SEND_REQMARKETDATATYPE =
    EClientErrors.CodeMsgPair(539, "Request Market Data Type Sending Error - ")
  val FAIL_SEND_REQPOSITIONS =
    EClientErrors.CodeMsgPair(540, "Request Positions Sending Error - ")
  val FAIL_SEND_CANPOSITIONS =
    EClientErrors.CodeMsgPair(541, "Cancel Positions Sending Error - ")
  val FAIL_SEND_REQACCOUNTDATA =
    EClientErrors.CodeMsgPair(542, "Request Account Data Sending Error - ")
  val FAIL_SEND_CANACCOUNTDATA =
    EClientErrors.CodeMsgPair(543, "Cancel Account Data Sending Error - ")
  val FAIL_SEND_VERIFYREQUEST =
    EClientErrors.CodeMsgPair(544, "Verify Request Sending Error - ")
  val FAIL_SEND_VERIFYMESSAGE =
    EClientErrors.CodeMsgPair(545, "Verify Message Sending Error - ")
  val FAIL_SEND_QUERYDISPLAYGROUPS =
    EClientErrors.CodeMsgPair(546, "Query Display Groups Sending Error - ")
  val FAIL_SEND_SUBSCRIBETOGROUPEVENTS =
    EClientErrors.CodeMsgPair(547, "Subscribe To Group Events Sending Error - ")
  val FAIL_SEND_UPDATEDISPLAYGROUP =
    EClientErrors.CodeMsgPair(548, "Update Display Group Sending Error - ")
  val FAIL_SEND_UNSUBSCRIBEFROMGROUPEVENTS = EClientErrors.CodeMsgPair(
    549,
    "Unsubscribe From Group Events Sending Error - "
  )
  val FAIL_SEND_STARTAPI =
    EClientErrors.CodeMsgPair(550, "Start API Sending Error - ")
  val FAIL_SEND_VERIFYANDAUTHREQUEST =
    EClientErrors.CodeMsgPair(551, "Verify And Auth Request Sending Error - ")
  val FAIL_SEND_VERIFYANDAUTHMESSAGE =
    EClientErrors.CodeMsgPair(552, "Verify And Auth Message Sending Error - ")
  val FAIL_SEND_REQPOSITIONSMULTI =
    EClientErrors.CodeMsgPair(553, "Request Positions Multi Sending Error - ")
  val FAIL_SEND_CANPOSITIONSMULTI =
    EClientErrors.CodeMsgPair(554, "Cancel Positions Multi Sending Error - ")
  val FAIL_SEND_REQACCOUNTUPDATESMULTI = EClientErrors.CodeMsgPair(
    555,
    "Request Account Updates Multi Sending Error - "
  )
  val FAIL_SEND_CANACCOUNTUPDATESMULTI = EClientErrors.CodeMsgPair(
    556,
    "Cancel Account Updates Multi Sending Error - "
  )
  val FAIL_SEND_REQSECDEFOPTPARAMS = EClientErrors.CodeMsgPair(
    557,
    "Request Security Definition Option Params Sending Error - "
  )
  val FAIL_SEND_REQSOFTDOLLARTIERS =
    EClientErrors.CodeMsgPair(558, "Request Soft Dollar Tiers Sending Error - ")
  val FAIL_SEND_REQFAMILYCODES =
    EClientErrors.CodeMsgPair(559, "Request Family Codes Sending Error - ")
  val FAIL_SEND_REQMATCHINGSYMBOLS =
    EClientErrors.CodeMsgPair(560, "Request Matching Symbols Sending Error - ")
  val FAIL_SEND_REQMKTDEPTHEXCHANGES = EClientErrors.CodeMsgPair(
    561,
    "Request Market Depth Exchanges Sending Error - "
  )
  val FAIL_SEND_REQSMARTCOMPONENTS =
    EClientErrors.CodeMsgPair(562, "Request Smart Components Sending Error - ")
  val FAIL_SEND_REQNEWSPROVIDERS =
    EClientErrors.CodeMsgPair(563, "Request News Providers Sending Error - ")
  val FAIL_SEND_REQNEWSARTICLE =
    EClientErrors.CodeMsgPair(564, "Request News Article Sending Error - ")
  val FAIL_SEND_REQHISTORICALNEWS =
    EClientErrors.CodeMsgPair(565, "Request Historical News Sending Error - ")
  val FAIL_SEND_REQHEADTIMESTAMP =
    EClientErrors.CodeMsgPair(566, "Request Head Time Stamp Sending Error - ")
  val FAIL_SEND_CANHEADTIMESTAMP =
    EClientErrors.CodeMsgPair(567, "Cancel Head Time Stamp Sending Error - ")
  val FAIL_SEND_REQMARKETRULE =
    EClientErrors.CodeMsgPair(568, "Request Market Rule Sending Error - ")
  val FAIL_SEND_REQPNL =
    EClientErrors.CodeMsgPair(566, "Request PnL Sending Error - ")
  val FAIL_SEND_CANPNL =
    EClientErrors.CodeMsgPair(567, "Cancel PnL Sending Error - ")
  val FAIL_SEND_REQPNL_SINGLE =
    EClientErrors.CodeMsgPair(568, "Request PnL Single Sending Error - ")
  val FAIL_SEND_CANPNL_SINGLE =
    EClientErrors.CodeMsgPair(569, "Cancel PnL Single Sending Error - ")
  val FAIL_SEND_HISTORICAL_TICK =
    EClientErrors.CodeMsgPair(569, "Request Historical Ticks Sending Error - ")
  val FAIL_SEND_REQTICKBYTICK =
    EClientErrors.CodeMsgPair(570, "Request Tick-By-Tick Sending Error - ")
  val FAIL_SEND_CANTICKBYTICK =
    EClientErrors.CodeMsgPair(571, "Cancel Tick-By-Tick Sending Error - ")
  val FAIL_SEND_REQ_COMPLETED_ORDERS =
    EClientErrors.CodeMsgPair(572, "Request Completed Orders Sending Error - ")
  val FAIL_SEND_REQ_WSH_META_DATA =
    EClientErrors.CodeMsgPair(573, "Request WSH Meta Data Sending Error - ")
  val FAIL_SEND_CAN_WSH_META_DATA =
    EClientErrors.CodeMsgPair(574, "Cancel WSH Meta Data Sending Error - ")
  val FAIL_SEND_REQ_WSH_EVENT_DATA =
    EClientErrors.CodeMsgPair(575, "Request WSH Event Data Sending Error - ")
  val FAIL_SEND_CAN_WSH_EVENT_DATA =
    EClientErrors.CodeMsgPair(576, "Cancel WSH Event Data Sending Error - ")
  val FAIL_SEND_REQUSERINFO =
    EClientErrors.CodeMsgPair(577, "Request User Info Sending Error - ")
  val INVALID_SYMBOL =
    EClientErrors.CodeMsgPair(579, "Invalid symbol in string - ")

}
