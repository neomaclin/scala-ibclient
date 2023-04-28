package org.quasigroup.ibclient.response

import MsgId.*
import readers.*

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.exceptions.EClientErrors
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.{*, given}

import org.quasigroup.ibclient.decoder.Decoder
import org.quasigroup.ibclient.decoder.Decoder.{*, given}
import org.quasigroup.ibclient.response.ResponseMsg.*

import cats.syntax.all.*
import scala.annotation.tailrec
import scala.util.Right
import scala.util.Try
import scala.annotation.switch

object MsgDecoders:

  inline given Decoder[VerifyCompleted] with
    private val create: DecoderState[VerifyCompleted] =
      for
        isSuccessfulStr <- read[String]
        errorText <- read[String]
      yield VerifyCompleted("true" == isSuccessfulStr, errorText)

    override def apply(
        entry: Array[String]
    ): Either[Throwable, VerifyCompleted] = create.runA(entry)

  end given

  inline def decode(entry: Array[String])(using serverVersion: IBClient.ServerVersion): Either[Throwable, ResponseMsg] =
    entry.headOption
      .map(_.toInt)
      .fold(
        Right(
          ErrorDetail(
            EClientErrors.NO_VALID_ID,
            EClientErrors.UNKNOWN_ID.code,
            EClientErrors.UNKNOWN_ID.msg,
            ""
          )
        )
      ) { msgid =>
        (msgid: @switch) match
          case END_CONN =>
            Right(ConnectionClosed)
          case TICK_PRICE =>
            TickPriceReader.create(using serverVersion).runA(entry.tail)
          case TICK_SIZE =>
            summon[Decoder[TickSize]](entry.tail.tail)
          case POSITION =>
            PositionMsgReader.create(using serverVersion).runA(entry.tail)
          case POSITION_END =>
            Right(PositionEnd)
          case ACCOUNT_SUMMARY =>
            summon[Decoder[AccountSummary]](entry.tail.tail)
          case ACCOUNT_SUMMARY_END =>
            summon[Decoder[AccountSummaryEnd]](entry.tail.tail)
          case TICK_OPTION_COMPUTATION =>
            TickOptionComputationReader.create(using serverVersion).runA(entry.tail)
          case TICK_GENERIC =>
            summon[Decoder[TickGeneric]](entry.tail.tail)
          case TICK_STRING =>
            summon[Decoder[TickString]](entry.tail.tail)
          case TICK_EFP =>
            summon[Decoder[TickEFP]](entry.tail.tail)
          case ORDER_STATUS =>
            OrderStatusMsgReader.create(using serverVersion).runA(entry.tail)
          case ACCT_VALUE =>
            UpdateAccountValueReader.create(using serverVersion).runA(entry.tail.tail)
          case PORTFOLIO_VALUE =>
            UpdatePortfolioReader.create(using serverVersion).runA(entry.tail)
          case ACCT_UPDATE_TIME =>
            summon[Decoder[UpdateAccountTime]](entry.tail.tail)
          case ERR_MSG =>
            summon[Decoder[ErrorDetail]](entry.tail)
          // case OPEN_ORDER =>
          //         summon[MsgDecoder[OpenOrder](msg)
          case NEXT_VALID_ID =>
            summon[Decoder[NextValidId]](entry.tail.tail)
          case SCANNER_DATA =>
            ScannerDataReader.create(using serverVersion).runA(entry.tail)
          case CONTRACT_DATA =>
            ContractDataReader.create(using serverVersion).runA(entry.tail)
          case BOND_CONTRACT_DATA =>
            BondContractDataReader.create(using serverVersion).runA(entry.tail)
          case EXECUTION_DATA =>
            ExecutionDataReader.create(using serverVersion).runA(entry.tail)
          case MARKET_DEPTH =>
            summon[Decoder[UpdateMktDepth]](entry.tail.tail)
          case MARKET_DEPTH_L2 =>
            summon[Decoder[UpdateMktDepthL2]](entry.tail.tail)
          // TODO: might be better with a reader
          case NEWS_BULLETINS =>
            summon[Decoder[UpdateNewsBulletin]](entry.tail.tail)
          case MANAGED_ACCTS =>
            summon[Decoder[ManagedAccounts]](entry.tail.tail)
          case RECEIVE_FA =>
            summon[Decoder[ReceiveFA]](entry.tail.tail)
          case HISTORICAL_DATA =>
            HistoricalDataReader.create(using serverVersion).runA(entry.tail)
          case SCANNER_PARAMETERS =>
            summon[Decoder[ScannerParameters]](entry.tail.tail)
          case CURRENT_TIME =>
            summon[Decoder[CurrentTime]](entry.tail.tail)
          case REAL_TIME_BARS =>
            summon[Decoder[RealtimeBar]](entry.tail.tail)
          case FUNDAMENTAL_DATA =>
            summon[Decoder[FundamentalData]](entry.tail.tail)
          case CONTRACT_DATA_END =>
            summon[Decoder[ContractDetailsEnd]](entry.tail.tail)
          case OPEN_ORDER_END =>
            Right(OpenOrderEnd)
          case ACCT_DOWNLOAD_END =>
            summon[Decoder[AccountDownloadEnd]](entry.tail.tail)
          case EXECUTION_DATA_END =>
            summon[Decoder[ExecDetailsEnd]](entry.tail.tail)
          case DELTA_NEUTRAL_VALIDATION =>
            summon[Decoder[DeltaNeutralValidation]](entry.tail.tail)
          case TICK_SNAPSHOT_END =>
            summon[Decoder[TickSnapshotEnd]](entry.tail.tail)
          case MARKET_DATA_TYPE =>
            summon[Decoder[MarketDataType]](entry.tail)
          case COMMISSION_REPORT =>
            summon[Decoder[CommissionReport]](entry.tail.tail).map(CommissionReportMsg(_))
          case VERIFY_MESSAGE_API =>
            summon[Decoder[VerifyMessageAPI]](entry.tail.tail)
          case VERIFY_COMPLETED =>
            summon[Decoder[VerifyCompleted]](entry.tail.tail)
          case DISPLAY_GROUP_LIST =>
            summon[Decoder[DisplayGroupList]](entry.tail.tail)
          case DISPLAY_GROUP_UPDATED =>
            summon[Decoder[DisplayGroupUpdated]](entry.tail.tail)
          case VERIFY_AND_AUTH_MESSAGE_API =>
            summon[Decoder[VerifyAndAuthMessageAPI]](entry.tail.tail)
          case VERIFY_AND_AUTH_COMPLETED =>
            summon[Decoder[VerifyCompleted]](entry.tail.tail)
              .map(v => VerifyAndAuthCompleted(v.isSuccessful, v.errorText))
          case POSITION_MULTI =>
            PositionMultiReader.create(using serverVersion).runA(entry.tail.tail)
          case POSITION_MULTI_END =>
            summon[Decoder[PositionMultiEnd]](entry.tail.tail)
          case ACCOUNT_UPDATE_MULTI =>
            summon[Decoder[AccountUpdateMulti]](entry.tail.tail)
          case ACCOUNT_UPDATE_MULTI_END =>
            summon[Decoder[AccountUpdateMultiEnd]](entry.tail.tail)
          case SECURITY_DEFINITION_OPTION_PARAMETER =>
            SecurityDefinitionOptionalParameterReader.create(using serverVersion).runA(entry.tail)
          case SECURITY_DEFINITION_OPTION_PARAMETER_END =>
            summon[Decoder[SecurityDefinitionOptionalParameterEnd]](entry.tail)
          case SOFT_DOLLAR_TIERS =>
            SoftDollarTiersReader.create(using serverVersion).runA(entry.tail)
          case FAMILY_CODES =>
            FamilyCodesReader.create(using serverVersion).runA(entry.tail)
          case SMART_COMPONENTS =>
            SmartComponentsReader.create(using serverVersion).runA(entry.tail)
          case TICK_REQ_PARAMS =>
            summon[Decoder[TickReqParams]](entry.tail)
          case SYMBOL_SAMPLES =>
            SymbolSamplesReader.create(using serverVersion).runA(entry.tail)
          case MKT_DEPTH_EXCHANGES =>
            MktDepthExchangesReader.create(using serverVersion).runA(entry.tail)
          case HEAD_TIMESTAMP =>
            summon[Decoder[HeadTimestamp]](entry.tail)
          case TICK_NEWS =>
            summon[Decoder[TickNews]](entry.tail)
          case NEWS_PROVIDERS =>
            NewsProvidersReader.create(using serverVersion).runA(entry.tail)
          case NEWS_ARTICLE =>
            summon[Decoder[NewsArticle]](entry.tail)
          case HISTORICAL_NEWS =>
            summon[Decoder[HistoricalNews]](entry.tail)
          case HISTORICAL_NEWS_END =>
            summon[Decoder[HistoricalNewsEnd]](entry.tail)
          case HISTOGRAM_DATA =>
            HistogramDataReader.create(using serverVersion).runA(entry.tail)
          case HISTORICAL_DATA_UPDATE =>
            HistoricalDataUpdateReader.create(using serverVersion).runA(entry.tail)
          case REROUTE_MKT_DATA_REQ =>
            summon[Decoder[RerouteMktDataReq]](entry.tail)
          case REROUTE_MKT_DEPTH_REQ =>
            summon[Decoder[RerouteMktDepthReq]](entry.tail)
          case MARKET_RULE =>
            MarketRuleReader.create(using serverVersion).runA(entry.tail)
          case PNL =>
            PnLMsgReader.create(using serverVersion).runA(entry.tail)
          case PNL_SINGLE =>
            PnLSingleMsgReader.create(using serverVersion).runA(entry.tail)
          case HISTORICAL_TICKS =>
            HistoricalTicksReader.create(using serverVersion).runA(entry.tail)
          case HISTORICAL_TICKS_BID_ASK =>
            HistoricalTicksBidAskReader.create(using serverVersion).runA(entry.tail)
          case HISTORICAL_TICKS_LAST =>
            HistoricalTicksLastReader.create(using serverVersion).runA(entry.tail)
          case TICK_BY_TICK =>
            TickByTicksReader.create(using serverVersion).runA(entry.tail)
          case ORDER_BOUND =>
            summon[Decoder[OrderBound]](entry.tail)
          // case COMPLETED_ORDER =>
          //         summon[MsgDecoder[CompletedOrder](msg)
          case COMPLETED_ORDERS_END =>
            Right(CompletedOrdersEnd)
          case REPLACE_FA_END =>
            summon[Decoder[ReplaceFAEnd]](entry.tail)
          case WSH_META_DATA =>
            summon[Decoder[WshMetaDataMsg]](entry.tail)
          case WSH_EVENT_DATA =>
            summon[Decoder[WshEventDataMsg]](entry.tail)
          case HISTORICAL_SCHEDULE =>
            HistoricalScheduleReader.create(using serverVersion).runA(entry.tail)
          case USER_INFO =>
            summon[Decoder[UserInfo]](entry.tail)
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
