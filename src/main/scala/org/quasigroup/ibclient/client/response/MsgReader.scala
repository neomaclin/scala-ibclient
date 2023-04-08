package org.quasigroup.ibclient.client.response

import cats.syntax.all.*
import org.quasigroup.ibclient.client.EClientErrors
import org.quasigroup.ibclient.client.decoder.Decoder
import org.quasigroup.ibclient.client.response.MsgId.*
import org.quasigroup.ibclient.client.response.ResponseMsg.*
import org.quasigroup.ibclient.client.types.*

import scala.annotation.tailrec
object MsgReader:

  given Decoder[FamilyCodes] = new Decoder[FamilyCodes]:
    @tailrec
    private def buildFamilyCodes(
        entry: List[String],
        acc: List[FamilyCode]
    ): List[FamilyCode] =
      entry match
        case accountID :: familyCodeStr :: xs =>
          buildFamilyCodes(xs, FamilyCode(accountID, familyCodeStr) :: acc)
        case Nil      => acc.reverse
        case _ :: Nil => Nil
      end match

    override def apply(entry: Array[String]): Either[Throwable, FamilyCodes] =
      entry match
        case Array(numberOfFamilyCode, rest: _*) =>
          if numberOfFamilyCode == "1" then Right(FamilyCodes(Nil))
          else Right(FamilyCodes(buildFamilyCodes(rest.toList, Nil)))
        case _ => Left(new Exception("nothing to decode"))
      end match
  end given

  def read(msgId: Int, msg: Array[String]): Either[Throwable, ResponseMsg] = {
    msgId match
      case END_CONN =>
        Right[Throwable, ResponseMsg](ConnectionClosed)
      // case TICK_PRICE =>
      //    Decoder.decode[TickPrice](msg)
      // case TICK_SIZE =>
      //    Decoder.decode[TickSize](msg)
      // case POSITION =>
      //   Decoder.decode[Position](msg)
      // case POSITION_END =>
      //   Decoder.decode[PositionEnd](msg)
      // case ACCOUNT_SUMMARY =>
      //    Decoder.decode[AccountSummary](msg)
      // case ACCOUNT_SUMMARY_END =>
      //   Decoder.decode[AccountSummaryEnd](msg)
      // case TICK_OPTION_COMPUTATION =>
      //   Decoder.decode[TickOptionComputation](msg)
      // case TICK_GENERIC =>
      //   Decoder.decode[TickGeneric](msg)
      // case TICK_STRING =>
      //         Decoder.decode[TickString](msg)
      // case TICK_EFP =>
      //         Decoder.decode[TickEFP](msg)
      // case ORDER_STATUS =>
      //         Decoder.decode[OrderStatus](msg)
      // case ACCT_VALUE =>
      //         Decoder.decode[AcctValue](msg)
      // case PORTFOLIO_VALUE =>
      //         Decoder.decode[PortfolioValue](msg)
      // case ACCT_UPDATE_TIME =>
      //         Decoder.decode[AcctUpdateTime](msg)
      case ERR_MSG =>
        Decoder.decode[ErrorDetail](msg)
      // case OPEN_ORDER =>
      //         Decoder.decode[OpenOrder](msg)
      case NEXT_VALID_ID =>
        Decoder.decode[NextValidId](msg.tail)
      //  case SCANNER_DATA =>
      //         Decoder.decode[ScannerData](msg)
      // case CONTRACT_DATA =>
      //         Decoder.decode[ContractData](msg)
      // case BOND_CONTRACT_DATA =>
      //         Decoder.decode[BondContractData](msg)
      // case EXECUTION_DATA =>
      //         Decoder.decode[ExecutionData](msg)
      // case MARKET_DEPTH =>
      //         Decoder.decode[MarketDepth](msg)
      // case MARKET_DEPTH_L2 =>
      //         Decoder.decode[MarketDepthL2](msg)
      // case NEWS_BULLETINS =>
      //         Decoder.decode[NewsBulletins](msg)
      case MANAGED_ACCTS =>
        Decoder.decode[ManagedAccounts](msg)
      // case RECEIVE_FA =>
      //         Decoder.decode[ReceiveFa](msg)
      // case HISTORICAL_DATA =>
      //         Decoder.decode[HistoricalData](msg)
      // case SCANNER_PARAMETERS =>
      //         Decoder.decode[ScannerParameters](msg)
      case CURRENT_TIME =>
        Decoder.decode[CurrentTime](msg.tail)
      // case REAL_TIME_BARS =>
      //         Decoder.decode[RealTimeBars](msg)
      // case FUNDAMENTAL_DATA =>
      //         Decoder.decode[FundamentalData](msg)
      // case CONTRACT_DATA_END =>
      //         Decoder.decode[ContractDataEnd](msg)
      // case OPEN_ORDER_END =>
      //         Decoder.decode[OpenOrderEnd](msg)
      // case ACCT_DOWNLOAD_END =>
      //         Decoder.decode[AcctDownloadEnd](msg)
      // case EXECUTION_DATA_END =>
      //         Decoder.decode[ExecutionDataEnd](msg)
      // case DELTA_NEUTRAL_VALIDATION =>
      //         Decoder.decode[DeltaNeutralValidation](msg)
      // case TICK_SNAPSHOT_END =>
      //         Decoder.decode[TickSnapshotEnd](msg)
      // case MARKET_DATA_TYPE =>
      //         Decoder.decode[MarketDataType](msg)
      // case COMMISSION_REPORT =>
      //         Decoder.decode[CommissionReport](msg)
      // case VERIFY_MESSAGE_API =>
      //         Decoder.decode[VerifyMessageApi](msg)
      // case VERIFY_COMPLETED =>
      //         Decoder.decode[VerifyCompleted](msg)
      // case DISPLAY_GROUP_LIST =>
      //         Decoder.decode[DisplayGroupList](msg)
      // case DISPLAY_GROUP_UPDATED =>
      //         Decoder.decode[DisplayGroupUpdated](msg)
      // case VERIFY_AND_AUTH_MESSAGE_API =>
      //         Decoder.decode[VerifyAndAuthMessage](msg)
      // case VERIFY_AND_AUTH_COMPLETED =>
      //         Decoder.decode[VerifyAndAuthCompleted](msg)
      // case POSITION_MULTI =>
      //         Decoder.decode[PositionMulti](msg)
      // case POSITION_MULTI_END =>
      //         Decoder.decode[PositionMultiEnd](msg)
      // case ACCOUNT_UPDATE_MULTI =>
      //         Decoder.decode[AccountUpdateMulti](msg)
      // case ACCOUNT_UPDATE_MULTI_END =>
      //         Decoder.decode[AccountUpdateMultiEnd](msg)
      // case SECURITY_DEFINITION_OPTION_PARAMETER =>
      //         Decoder.decode[SecurityDefinitionOptionalParameter](msg)
      // case SECURITY_DEFINITION_OPTION_PARAMETER_END =>
      //         Decoder.decode[SecurityDefinitionOptionalParameterEnd](msg)
      // case SOFT_DOLLAR_TIERS =>
      //         Decoder.decode[SoftDollarTiers](msg)
      case FAMILY_CODES =>
        Decoder.decode[FamilyCodes](msg)
      // case SMART_COMPONENTS =>
      //         Decoder.decode[SmartComponents](msg)
      // case TICK_REQ_PARAMS =>
      //         Decoder.decode[TickReqParams](msg)
      // case SYMBOL_SAMPLES =>
      //         Decoder.decode[SymbolSamples](msg)
      // case MKT_DEPTH_EXCHANGES =>
      //         Decoder.decode[MktDepthExchanges](msg)
      // case HEAD_TIMESTAMP =>
      //         Decoder.decode[HeadTimestamp](msg)
      // case TICK_NEWS =>
      //         Decoder.decode[TickNews](msg)
      // case NEWS_PROVIDERS =>
      //         Decoder.decode[NewsProviders](msg)
      // case NEWS_ARTICLE =>
      //         Decoder.decode[NewsArticle](msg)
      // case HISTORICAL_NEWS =>
      //         Decoder.decode[HistoricalNews](msg)
      // case HISTORICAL_NEWS_END =>
      //         Decoder.decode[HistoricalNewsEnd](msg)
      // case HISTOGRAM_DATA =>
      //         Decoder.decode[HistogramData](msg)
      // case HISTORICAL_DATA_UPDATE =>
      //         Decoder.decode[HistoricalDataUpdate](msg)
      // case REROUTE_MKT_DATA_REQ =>
      //         Decoder.decode[RerouteMktDataReq](msg)
      // case REROUTE_MKT_DEPTH_REQ =>
      //         Decoder.decode[RerouteMktDepthReq](msg)
      // case MARKET_RULE =>
      //         Decoder.decode[MarketRule](msg)
      // case PNL =>
      //         Decoder.decode[PnL](msg)
      // case PNL_SINGLE =>
      //         Decoder.decode[PnLSingle](msg)
      // case HISTORICAL_TICKS =>
      //         Decoder.decode[HistoricalTicks](msg)
      // case HISTORICAL_TICKS_BID_ASK =>
      //         Decoder.decode[HistoricalTicksBidAsk](msg)
      // case HISTORICAL_TICKS_LAST =>
      //         Decoder.decode[HistoricalTicksLast](msg)
      // case TICK_BY_TICK =>
      //         Decoder.decode[TickByTick](msg)
      // case ORDER_BOUND =>
      //         Decoder.decode[OrderBound](msg)
      // case COMPLETED_ORDER =>
      //         Decoder.decode[CompletedOrder](msg)
      // case COMPLETED_ORDERS_END =>
      //         Decoder.decode[CompletedOrdersEnd](msg)
      // case REPLACE_FA_END =>
      //         Decoder.decode[ReplaceFAEnd](msg)
      // case WSH_META_DATA =>
      //         Decoder.decode[WshMetaData](msg)
      // case WSH_EVENT_DATA =>
      //         Decoder.decode[WshEventData](msg)
      // case HISTORICAL_SCHEDULE =>
      //   Decoder.decode[HistoricalSchedule](msg)
      // case USER_INFO =>
      //         Decoder.decode[UserInfo](msg)
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
