package org.quasigroup.ibclient.client.response

import cats.syntax.all.*
import org.quasigroup.ibclient.client.EClientErrors
import org.quasigroup.ibclient.client.decoder.Decoder
import org.quasigroup.ibclient.client.decoder.Decoder.*
import org.quasigroup.ibclient.client.response.MsgId.*
import org.quasigroup.ibclient.client.response.ResponseMsg.*
import org.quasigroup.ibclient.client.response.MsgDecoders.given
import org.quasigroup.ibclient.client.types.*

import scala.annotation.tailrec
object MsgReader:

  def read(msgId: Int, msg: Array[String]): Either[Throwable, ResponseMsg] = {
    msgId match
      case END_CONN =>
        Right(ConnectionClosed)
      case TICK_PRICE =>
        Decoder.decode[TickPrice](msg)
      case TICK_SIZE =>
        Decoder.decode[TickSize](msg.tail)
      case POSITION =>
        Decoder.decode[Position](msg)
      case POSITION_END =>
        Right(PositionEnd)
      case ACCOUNT_SUMMARY =>
        Decoder.decode[AccountSummary](msg.tail)
      case ACCOUNT_SUMMARY_END =>
        Decoder.decode[AccountSummaryEnd](msg.tail)
      // case TICK_OPTION_COMPUTATION =>
      //   Decoder.decode[TickOptionComputation](msg)
      case TICK_GENERIC =>
        Decoder.decode[TickGeneric](msg.tail)
      case TICK_STRING =>
        Decoder.decode[TickString](msg.tail)
      case TICK_EFP =>
        Decoder.decode[TickEFP](msg.tail)
      case ORDER_STATUS =>
        Decoder.decode[OrderStatus](msg)
      case ACCT_VALUE =>
        Decoder.decode[UpdateAccountValue](msg.tail)
      // case PORTFOLIO_VALUE =>
      //         Decoder.decode[PortfolioValue](msg)
      case ACCT_UPDATE_TIME =>
        Decoder.decode[UpdateAccountTime](msg.tail)
      case ERR_MSG =>
        Decoder.decode[ErrorDetail](msg)
      // case OPEN_ORDER =>
      //         Decoder.decode[OpenOrder](msg)
      case NEXT_VALID_ID =>
        Decoder.decode[NextValidId](msg.tail)
      // case SCANNER_DATA =>
      //         Decoder.decode[ScannerData](msg)
      // case CONTRACT_DATA =>
      //         Decoder.decode[ContractData](msg)
      // case BOND_CONTRACT_DATA =>
      //         Decoder.decode[BondContractData](msg)
      // case EXECUTION_DATA =>
      //         Decoder.decode[ExecutionData](msg)
      case MARKET_DEPTH =>
        Decoder.decode[UpdateMktDepth](msg.tail)
      case MARKET_DEPTH_L2 =>
        Decoder.decode[UpdateMktDepthL2](msg.tail)
      case NEWS_BULLETINS =>
        Decoder.decode[UpdateNewsBulletin](msg.tail)
      case MANAGED_ACCTS =>
        Decoder.decode[ManagedAccounts](msg.tail)
      case RECEIVE_FA =>
        Decoder.decode[ReceiveFA](msg.tail)
      // case HISTORICAL_DATA =>
      //         Decoder.decode[HistoricalData](msg)
      case SCANNER_PARAMETERS =>
        Decoder.decode[ScannerParameters](msg.tail)
      case CURRENT_TIME =>
        Decoder.decode[CurrentTime](msg.tail)
      case REAL_TIME_BARS =>
        Decoder.decode[RealtimeBar](msg.tail)
      case FUNDAMENTAL_DATA =>
        Decoder.decode[FundamentalData](msg.tail)
      case CONTRACT_DATA_END =>
        Decoder.decode[ContractDetailsEnd](msg.tail)
      case OPEN_ORDER_END =>
        Right(OpenOrderEnd)
      case ACCT_DOWNLOAD_END =>
        Decoder.decode[AccountDownloadEnd](msg.tail)
      case EXECUTION_DATA_END =>
        Decoder.decode[ExecDetailsEnd](msg.tail)
      case DELTA_NEUTRAL_VALIDATION =>
        Decoder.decode[DeltaNeutralValidation](msg.tail)
      case TICK_SNAPSHOT_END =>
        Decoder.decode[TickSnapshotEnd](msg.tail)
      case MARKET_DATA_TYPE =>
        Decoder.decode[MarketDataType](msg)
      case COMMISSION_REPORT =>
        Decoder.decode[CommissionReportMsg](msg.tail)
      case VERIFY_MESSAGE_API =>
        Decoder.decode[VerifyMessageAPI](msg.tail)
      case VERIFY_COMPLETED =>
        Decoder.decode[VerifyCompleted](msg.tail)
      case DISPLAY_GROUP_LIST =>
        Decoder.decode[DisplayGroupList](msg.tail)
      case DISPLAY_GROUP_UPDATED =>
        Decoder.decode[DisplayGroupUpdated](msg.tail)
      case VERIFY_AND_AUTH_MESSAGE_API =>
        Decoder.decode[VerifyAndAuthMessageAPI](msg.tail)
      case VERIFY_AND_AUTH_COMPLETED =>
        Decoder.decode[VerifyAndAuthCompleted](msg.tail)
      case POSITION_MULTI =>
        Decoder.decode[PositionMulti](msg.tail)
      case POSITION_MULTI_END =>
        Decoder.decode[PositionMultiEnd](msg.tail)
      case ACCOUNT_UPDATE_MULTI =>
        Decoder.decode[AccountUpdateMulti](msg.tail)
      case ACCOUNT_UPDATE_MULTI_END =>
        Decoder.decode[AccountUpdateMultiEnd](msg.tail)
      case SECURITY_DEFINITION_OPTION_PARAMETER =>
        Decoder.decode[SecurityDefinitionOptionalParameter](msg)
      case SECURITY_DEFINITION_OPTION_PARAMETER_END =>
        Decoder.decode[SecurityDefinitionOptionalParameterEnd](msg)
      case SOFT_DOLLAR_TIERS =>
        Decoder.decode[SoftDollarTiers](msg)
      case FAMILY_CODES =>
        Decoder.decode[FamilyCodes](msg)
      case SMART_COMPONENTS =>
        Decoder.decode[SmartComponents](msg)
      case TICK_REQ_PARAMS =>
        Decoder.decode[TickReqParams](msg)
      // case SYMBOL_SAMPLES =>
      //         Decoder.decode[SymbolSamples](msg)
      case MKT_DEPTH_EXCHANGES =>
        Decoder.decode[MktDepthExchanges](msg)
      case HEAD_TIMESTAMP =>
        Decoder.decode[HeadTimestamp](msg)
      case TICK_NEWS =>
        Decoder.decode[TickNews](msg)
      case NEWS_PROVIDERS =>
        Decoder.decode[NewsProviders](msg)
      case NEWS_ARTICLE =>
        Decoder.decode[NewsArticle](msg)
      case HISTORICAL_NEWS =>
        Decoder.decode[HistoricalNews](msg)
      case HISTORICAL_NEWS_END =>
        Decoder.decode[HistoricalNewsEnd](msg)
      // case HISTOGRAM_DATA =>
      //         Decoder.decode[HistogramData](msg)
      // case HISTORICAL_DATA_UPDATE =>
      //         Decoder.decode[HistoricalDataUpdate](msg)
      case REROUTE_MKT_DATA_REQ =>
        Decoder.decode[RerouteMktDataReq](msg)
      case REROUTE_MKT_DEPTH_REQ =>
        Decoder.decode[RerouteMktDepthReq](msg)
      case MARKET_RULE =>
        Decoder.decode[MarketRule](msg)
      case PNL =>
        Decoder.decode[PnL](msg)
      case PNL_SINGLE =>
        Decoder.decode[PnLSingle](msg)
      // case HISTORICAL_TICKS =>
      //         Decoder.decode[HistoricalTicks](msg)
      // case HISTORICAL_TICKS_BID_ASK =>
      //         Decoder.decode[HistoricalTicksBidAsk](msg)
      // case HISTORICAL_TICKS_LAST =>
      //         Decoder.decode[HistoricalTicksLast](msg)
      // case TICK_BY_TICK =>
      //         Decoder.decode[TickByTick](msg)
      case ORDER_BOUND =>
        Decoder.decode[OrderBound](msg)
      // case COMPLETED_ORDER =>
      //         Decoder.decode[CompletedOrder](msg)
      case COMPLETED_ORDERS_END =>
        Right(CompletedOrdersEnd)
      case REPLACE_FA_END =>
        Decoder.decode[ReplaceFAEnd](msg)
      case WSH_META_DATA =>
        Decoder.decode[WshMetaData](msg)
      case WSH_EVENT_DATA =>
        Decoder.decode[WshEventData](msg)
      case HISTORICAL_SCHEDULE =>
        Decoder.decode[HistoricalSchedule](msg)
      case USER_INFO =>
        Decoder.decode[UserInfo](msg)
      case _ =>
        Right(
          ErrorDetail(
            EClientErrors.NO_VALID_ID,
            EClientErrors.UNKNOWN_ID.code,
            EClientErrors.UNKNOWN_ID.msg,
            ""
          )
        )
    end match

  }

end MsgReader
