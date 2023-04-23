package org.quasigroup.ibclient.response

import MsgId.*
import readers.*
import org.quasigroup.ibclient.exceptions.EClientErrors
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.{*, given}

import org.quasigroup.ibclient.decoder.Decoder
import org.quasigroup.ibclient.decoder.Decoder.*
import org.quasigroup.ibclient.response.ResponseMsg.*

import cats.syntax.all.*
import scala.annotation.tailrec
import scala.util.Right
import scala.util.Try

object MsgDecoders:
  inline given Decoder[UpdatePortfolio] = UpdatePortfolioReader.create.runA(_)

  inline given Decoder[TickPrice] = TickPriceReader.create.runA(_)

  inline given Decoder[TickOptionComputation] = TickOptionComputationReader.create.runA(_)

  inline given Decoder[PositionMsg] = PositionMsgReader.create.runA(_)

  inline given Decoder[PositionMulti] = PositionMultiReader.create.runA(_)

  inline given Decoder[SecurityDefinitionOptionalParameter] =  SecurityDefinitionOptionalParameterReader.create.runA(_)

  inline given Decoder[CommissionReportMsg] =
    summon[Decoder[CommissionReport]].map(CommissionReportMsg(_))

  inline given Decoder[VerifyAndAuthCompleted] =
    summon[Decoder[VerifyCompleted]].map(v => VerifyAndAuthCompleted(v.isSuccessful, v.errorText))
  end given

  inline given Decoder[VerifyCompleted] with
    private val createVerifyCompleted: DecoderState[VerifyCompleted] =
      for
        isSuccessfulStr <- read[String]
        errorText <- read[String]
      yield VerifyCompleted("true" == isSuccessfulStr, errorText)

    override def apply(
        entry: Array[String]
    ): Either[Throwable, VerifyCompleted] = createVerifyCompleted.runA(entry)

  end given

//  inline given Decoder[DeltaNeutralValidation] with
//    private val createDeltaNeutralValidation: DecoderState[DeltaNeutralValidation] =
//      for
//        reqId <- read[Int]
//        conid <- read[Int]
//        delta <- read[Double]
//        price <- read[Double]
//      yield DeltaNeutralValidation(
//        reqId,
//        DeltaNeutralContract(conid, delta, price)
//      )
//
//    override def apply(
//        entry: Array[String]
//    ): Either[Throwable, DeltaNeutralValidation] =
//      createDeltaNeutralValidation.runA(entry)
//  end given

  inline given Decoder[HistoricalTicks] = HistoricalTicksReader.create.runA(_)

  inline given Decoder[HistoricalTicksBidAsk] = HistoricalTicksBidAskReader.create.runA(_)

  inline given Decoder[HistoricalTicksLast] = HistoricalTicksLastReader.create.runA(_)

  inline given Decoder[HistoricalDataUpdate] = HistoricalDataUpdateReader.create.runA(_)

  inline given Decoder[HistoricalData] = HistoricalDataReader.create.runA(_)
  inline given Decoder[HistoricalSchedule] = HistoricalScheduleReader.create.runA(_)

  inline given Decoder[HistogramData] = HistogramDataReader.create.runA(_)
  
  inline given Decoder[SymbolSamples] = SymbolSamplesReader.create.runA(_)

  inline given Decoder[FamilyCodes] =  FamilyCodesReader.create.runA(_)

  inline given Decoder[SmartComponents] = SmartComponentsReader.create.runA(_)

  inline given Decoder[MktDepthExchanges] = MktDepthExchangesReader.create.runA(_)

  inline given Decoder[NewsProviders] = NewsProvidersReader.create.runA(_)

  inline given Decoder[SoftDollarTiers] = SoftDollarTiersReader.create.runA(_)

  inline given Decoder[ScannerData] = ScannerDataReader.create.runA(_)

  inline given Decoder[MarketRule] = MarketRuleReader.create.runA(_)

  inline given Decoder[ResponseMsg] with
    def apply(entry: Array[String]): Either[Throwable, ResponseMsg] =
      entry.headOption.map(_.toInt).fold(Right(
        ErrorDetail(
          EClientErrors.NO_VALID_ID,
          EClientErrors.UNKNOWN_ID.code,
          EClientErrors.UNKNOWN_ID.msg,
          ""
        )
      )) {
        case END_CONN =>
          Right(ConnectionClosed)
        case TICK_PRICE =>
          Decoder.decode[TickPrice](entry.tail)
        case TICK_SIZE =>
          Decoder.decode[TickSize](entry.tail.tail)
        case POSITION =>
          Decoder.decode[PositionMsg](entry.tail)
        case POSITION_END =>
          Right(PositionEnd)
        case ACCOUNT_SUMMARY =>
          Decoder.decode[AccountSummary](entry.tail.tail)
        case ACCOUNT_SUMMARY_END =>
          Decoder.decode[AccountSummaryEnd](entry.tail.tail)
        case TICK_OPTION_COMPUTATION =>
          Decoder.decode[TickOptionComputation](entry.tail)
        case TICK_GENERIC =>
          Decoder.decode[TickGeneric](entry.tail.tail)
        case TICK_STRING =>
          Decoder.decode[TickString](entry.tail.tail)
        case TICK_EFP =>
          Decoder.decode[TickEFP](entry.tail.tail)
        case ORDER_STATUS =>
          Decoder.decode[OrderStatus](entry.tail)
        case ACCT_VALUE =>
          Decoder.decode[UpdateAccountValue](entry.tail.tail)
        case PORTFOLIO_VALUE =>
          Decoder.decode[UpdatePortfolio](entry.tail)
        case ACCT_UPDATE_TIME =>
          Decoder.decode[UpdateAccountTime](entry.tail.tail)
        case ERR_MSG =>
          Decoder.decode[ErrorDetail](entry.tail)
        // case OPEN_ORDER =>
        //         Decoder.decode[OpenOrder](msg)
        case NEXT_VALID_ID =>
          Decoder.decode[NextValidId](entry.tail.tail)
        case SCANNER_DATA =>
          Decoder.decode[ScannerData](entry.tail)
        // case CONTRACT_DATA =>
        //         Decoder.decode[ContractData](msg)
        // case BOND_CONTRACT_DATA =>
        //         Decoder.decode[BondContractData](msg)
        // case EXECUTION_DATA =>
        //         Decoder.decode[ExecutionData](msg)
        case MARKET_DEPTH =>
          Decoder.decode[UpdateMktDepth](entry.tail.tail)
        case MARKET_DEPTH_L2 =>
          Decoder.decode[UpdateMktDepthL2](entry.tail.tail)
        case NEWS_BULLETINS =>
          Decoder.decode[UpdateNewsBulletin](entry.tail.tail)
        case MANAGED_ACCTS =>
          Decoder.decode[ManagedAccounts](entry.tail.tail)
        case RECEIVE_FA =>
          Decoder.decode[ReceiveFA](entry.tail.tail)
        case HISTORICAL_DATA =>
          Decoder.decode[HistoricalData](entry.tail)
        case SCANNER_PARAMETERS =>
          Decoder.decode[ScannerParameters](entry.tail.tail)
        case CURRENT_TIME =>
          Decoder.decode[CurrentTime](entry.tail.tail)
        case REAL_TIME_BARS =>
          Decoder.decode[RealtimeBar](entry.tail.tail)
        case FUNDAMENTAL_DATA =>
          Decoder.decode[FundamentalData](entry.tail.tail)
        case CONTRACT_DATA_END =>
          Decoder.decode[ContractDetailsEnd](entry.tail.tail)
        case OPEN_ORDER_END =>
          Right(OpenOrderEnd)
        case ACCT_DOWNLOAD_END =>
          Decoder.decode[AccountDownloadEnd](entry.tail.tail)
        case EXECUTION_DATA_END =>
          Decoder.decode[ExecDetailsEnd](entry.tail.tail)
        case DELTA_NEUTRAL_VALIDATION =>
          Decoder.decode[DeltaNeutralValidation](entry.tail.tail)
        case TICK_SNAPSHOT_END =>
          Decoder.decode[TickSnapshotEnd](entry.tail.tail)
        case MARKET_DATA_TYPE =>
          Decoder.decode[MarketDataType](entry.tail)
        case COMMISSION_REPORT =>
          Decoder.decode[CommissionReportMsg](entry.tail.tail)
        case VERIFY_MESSAGE_API =>
          Decoder.decode[VerifyMessageAPI](entry.tail.tail)
        case VERIFY_COMPLETED =>
          Decoder.decode[VerifyCompleted](entry.tail.tail)
        case DISPLAY_GROUP_LIST =>
          Decoder.decode[DisplayGroupList](entry.tail.tail)
        case DISPLAY_GROUP_UPDATED =>
          Decoder.decode[DisplayGroupUpdated](entry.tail.tail)
        case VERIFY_AND_AUTH_MESSAGE_API =>
          Decoder.decode[VerifyAndAuthMessageAPI](entry.tail.tail)
        case VERIFY_AND_AUTH_COMPLETED =>
          Decoder.decode[VerifyAndAuthCompleted](entry.tail.tail)
        case POSITION_MULTI =>
          Decoder.decode[PositionMulti](entry.tail.tail)
        case POSITION_MULTI_END =>
          Decoder.decode[PositionMultiEnd](entry.tail.tail)
        case ACCOUNT_UPDATE_MULTI =>
          Decoder.decode[AccountUpdateMulti](entry.tail.tail)
        case ACCOUNT_UPDATE_MULTI_END =>
          Decoder.decode[AccountUpdateMultiEnd](entry.tail.tail)
        case SECURITY_DEFINITION_OPTION_PARAMETER =>
          Decoder.decode[SecurityDefinitionOptionalParameter](entry.tail)
        case SECURITY_DEFINITION_OPTION_PARAMETER_END =>
          Decoder.decode[SecurityDefinitionOptionalParameterEnd](entry.tail)
        case SOFT_DOLLAR_TIERS =>
          Decoder.decode[SoftDollarTiers](entry.tail)
        case FAMILY_CODES =>
          Decoder.decode[FamilyCodes](entry.tail)
        case SMART_COMPONENTS =>
          Decoder.decode[SmartComponents](entry.tail)
        case TICK_REQ_PARAMS =>
          Decoder.decode[TickReqParams](entry.tail)
        case SYMBOL_SAMPLES =>
          Decoder.decode[SymbolSamples](entry.tail)
        case MKT_DEPTH_EXCHANGES =>
          Decoder.decode[MktDepthExchanges](entry.tail)
        case HEAD_TIMESTAMP =>
          Decoder.decode[HeadTimestamp](entry.tail)
        case TICK_NEWS =>
          Decoder.decode[TickNews](entry.tail)
        case NEWS_PROVIDERS =>
          Decoder.decode[NewsProviders](entry.tail)
        case NEWS_ARTICLE =>
          Decoder.decode[NewsArticle](entry.tail)
        case HISTORICAL_NEWS =>
          Decoder.decode[HistoricalNews](entry.tail)
        case HISTORICAL_NEWS_END =>
          Decoder.decode[HistoricalNewsEnd](entry.tail)
        case HISTOGRAM_DATA =>
          Decoder.decode[HistogramData](entry.tail)
        case HISTORICAL_DATA_UPDATE =>
          Decoder.decode[HistoricalDataUpdate](entry.tail)
        case REROUTE_MKT_DATA_REQ =>
          Decoder.decode[RerouteMktDataReq](entry.tail)
        case REROUTE_MKT_DEPTH_REQ =>
          Decoder.decode[RerouteMktDepthReq](entry.tail)
        case MARKET_RULE =>
          Decoder.decode[MarketRule](entry.tail)
        case PNL =>
          Decoder.decode[PnL](entry.tail)
        case PNL_SINGLE =>
          Decoder.decode[PnLSingle](entry.tail)
        case HISTORICAL_TICKS =>
          Decoder.decode[HistoricalTicks](entry.tail)
        case HISTORICAL_TICKS_BID_ASK =>
          Decoder.decode[HistoricalTicksBidAsk](entry.tail)
        case HISTORICAL_TICKS_LAST =>
          Decoder.decode[HistoricalTicksLast](entry.tail)
        case TICK_BY_TICK =>
          TickByTicksReader.create.runA(entry.tail)
        case ORDER_BOUND =>
          Decoder.decode[OrderBound](entry.tail)
        // case COMPLETED_ORDER =>
        //         Decoder.decode[CompletedOrder](msg)
        case COMPLETED_ORDERS_END =>
          Right(CompletedOrdersEnd)
        case REPLACE_FA_END =>
          Decoder.decode[ReplaceFAEnd](entry.tail)
        case WSH_META_DATA =>
          Decoder.decode[WshMetaDataMsg](entry.tail)
        case WSH_EVENT_DATA =>
          Decoder.decode[WshEventDataMsg](entry.tail)
        case HISTORICAL_SCHEDULE =>
          Decoder.decode[HistoricalSchedule](entry.tail)
        case USER_INFO =>
          Decoder.decode[UserInfo](entry.tail)
        case _ =>
          Right(
            ErrorDetail(
              EClientErrors.NO_VALID_ID,
              EClientErrors.UNKNOWN_ID.code,
              EClientErrors.UNKNOWN_ID.msg,
              ""
            )
          )
      }
  end given
