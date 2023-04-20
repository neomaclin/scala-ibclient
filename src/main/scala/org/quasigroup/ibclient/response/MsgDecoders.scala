package org.quasigroup.ibclient.response

import MsgId.*
import org.quasigroup.ibclient.exceptions.EClientErrors
import org.quasigroup.ibclient.types.*

import org.quasigroup.ibclient.decoder.Decoder
import org.quasigroup.ibclient.decoder.Decoder.*
import org.quasigroup.ibclient.response.ResponseMsg.*

import cats.syntax.option.*
import scala.annotation.tailrec
import scala.util.Right
import scala.util.Try

object MsgDecoders:

  inline def partiallyApply[T](
      entry: Array[String],
      matching: PartialFunction[Array[String], Either[Throwable, T]]
  ): Either[Throwable, T] =
    matching.applyOrElse(entry, _ => Left(new Exception("msg format error")))

  inline given Decoder[UpdatePortfolio] =
    MsgReader.createUpdatePortfolio.runA(_)

  inline given Decoder[MktDataType] =
    summon[Decoder[Int]].map(MktDataType.fromOrdinal)

  inline given Decoder[NewsType] =
    summon[Decoder[Int]].map(NewsType.fromOrdinal)

  inline given Decoder[TickType] =
    summon[Decoder[Int]].map(TickType.fromOrdinal)

  inline given Decoder[TickPrice] with
    def apply(
        entry: Array[String]
    ): Either[Throwable, TickPrice] =
      partiallyApply(
        entry,
        {
          case Array(
                version,
                tickerId,
                tickType,
                price,
                rest: _*
              ) =>
            val versionInt = version.toInt
            val tickerIdInt = tickerId.toInt
            val tickTypeInt = tickType.toInt
            val size =
              if versionInt >= 2 then
                Decimal.parse(rest.head).getOrElse(Decimal.INVALID)
              else Decimal.INVALID
            val nextMsgs = if versionInt >= 2 then rest.tail else rest
            val attribs =
              if versionInt >= 3 then
                val mask = BitMask(nextMsgs.head.toInt)
                TickAttrib(
                  canAutoExecute = mask.get(0),
                  pastLimit = mask.get(1),
                  preOpen = mask.get(2)
                )
              else TickAttrib()
            val optionalSizeTickType =
              if versionInt >= 2 then
                tickTypeInt match
                  case 1  => TickType.BID_SIZE.some
                  case 2  => TickType.ASK_SIZE.some
                  case 4  => TickType.LAST_SIZE.some
                  case 66 => TickType.DELAYED_BID_SIZE.some
                  case 67 => TickType.DELAYED_ASK_SIZE.some
                  case 68 => TickType.DELAYED_LAST_SIZE.some
                  case _  => None
              else None
            Right(
              TickPrice(
                tickerIdInt,
                TickType.fromOrdinal(tickTypeInt),
                price.toDouble,
                attribs,
                optionalSizeTickType.map(TickSize(tickerIdInt, _, size))
              )
            )

        }
      )

  end given

  inline given Decoder[TickOptionComputation] with
    def apply(
        entry: Array[String]
    ): Either[Throwable, TickOptionComputation] =
      partiallyApply(
        entry,
        {
          case Array(
                version,
                tickerId,
                tickType,
                impliedVol,
                delta,
                rest: _*
              ) =>
            val versionInt = version.toInt
            val tickerIdInt = tickerId.toInt
            val tickTypeEnum = TickType.fromOrdinal(tickType.toInt)
            val impliedVolDouble = Try(impliedVol.toDouble)
              .filter(_ != -1)
              .getOrElse(Double.MaxValue)
            val deltaDouble =
              Try(delta.toDouble).filter(_ != -2).getOrElse(Double.MaxValue)
            val (optPriceDouble, pvDividendDouble, otherRest) =
              if (
                versionInt >= 6 || tickTypeEnum == TickType.MODEL_OPTION || tickTypeEnum == TickType.DELAYED_MODEL_OPTION
              )
              then
                rest.toArray match
                  case Array(optPrice, pvDividend, remaining: _*) =>
                    val optPriceDouble = Try(optPrice.toDouble)
                      .filter(_ != -1)
                      .getOrElse(Double.MaxValue)
                    val pvDividendDouble = Try(pvDividend.toDouble)
                      .filter(_ != -1)
                      .getOrElse(Double.MaxValue)
                    (optPriceDouble, pvDividendDouble, remaining)
                  case _ => (Double.MaxValue, Double.MaxValue, rest)
              else (Double.MaxValue, Double.MaxValue, rest)
            val (gammaDouble, vegaDouble, thetaDouble, undPriceDouble) =
              if (versionInt >= 6) then
                rest.toArray match
                  case Array(gamma, vega, theta, undPrice) =>
                    val gammaDouble = Try(gamma.toDouble)
                      .filter(_ != -2)
                      .getOrElse(Double.MaxValue)
                    val vegaDouble = Try(vega.toDouble)
                      .filter(_ != -2)
                      .getOrElse(Double.MaxValue)
                    val thetaDouble = Try(theta.toDouble)
                      .filter(_ != -2)
                      .getOrElse(Double.MaxValue)
                    val undPriceDouble = Try(undPrice.toDouble)
                      .filter(_ != -1)
                      .getOrElse(Double.MaxValue)
                    (gammaDouble, vegaDouble, thetaDouble, undPriceDouble)
                  case _ =>
                    (
                      Double.MaxValue,
                      Double.MaxValue,
                      Double.MaxValue,
                      Double.MaxValue
                    )
              else
                (
                  Double.MaxValue,
                  Double.MaxValue,
                  Double.MaxValue,
                  Double.MaxValue
                )
            Right(
              TickOptionComputation(
                tickerIdInt,
                tickTypeEnum,
                impliedVolDouble,
                deltaDouble,
                optPriceDouble,
                pvDividendDouble,
                gammaDouble,
                vegaDouble,
                thetaDouble,
                undPriceDouble
              )
            )

        }
      )

  end given

  inline given Decoder[PositionMsg] with
    def apply(
        entry: Array[String]
    ): Either[Throwable, PositionMsg] =
      partiallyApply(
        entry,
        {
          case Array(
                version,
                account,
                conid,
                symbol,
                secType,
                lastTradeDateOrContractMonth,
                strike,
                right,
                multiplier,
                exchange,
                currency,
                localSymbol,
                rest: _*
              ) =>
            val versionInt = version.toInt
            val (tradingClass, nextRest) =
              if versionInt >= 2 then (rest.head, rest.tail) else ("", rest)
            val pos =
              Decimal.parse(nextRest.head).toOption.getOrElse(Decimal.INVALID)
            val avgCost =
              if versionInt >= 3 then nextRest.tail.head.toDouble else 0
            val contract = Contract(
              conid.toInt,
              symbol,
              SecType.valueOf(secType),
              lastTradeDateOrContractMonth,
              strike.toDouble,
              ContractRight.fromString(right),
              multiplier,
              exchange,
              currency,
              localSymbol,
              tradingClass
            )
            Right(PositionMsg(account, contract, pos, avgCost))
        }
      )

  end given

  inline given Decoder[PositionMulti] with
    def apply(
        entry: Array[String]
    ): Either[Throwable, PositionMulti] =
      partiallyApply(
        entry,
        {
          case Array(
                reqId,
                account,
                conid,
                symbol,
                secType,
                lastTradeDateOrContractMonth,
                strike,
                right,
                multiplier,
                exchange,
                currency,
                localSymbol,
                tradingClass,
                pos,
                avgCost,
                modelCode
              ) =>
            val posDecimal =
              Decimal.parse(pos).toOption.getOrElse(Decimal.INVALID)
            val contract = Contract(
              conid.toInt,
              symbol,
              SecType.valueOf(secType),
              lastTradeDateOrContractMonth,
              strike.toDouble,
              ContractRight.fromString(right),
              multiplier,
              exchange,
              currency,
              localSymbol,
              tradingClass
            )
            Right(
              PositionMulti(
                reqId.toInt,
                account,
                modelCode,
                contract,
                posDecimal,
                avgCost.toDouble
              )
            )
        }
      )

  end given

  inline given Decoder[SecurityDefinitionOptionalParameter] with
    def apply(
        entry: Array[String]
    ): Either[Throwable, SecurityDefinitionOptionalParameter] =
      partiallyApply(
        entry,
        {
          case Array(
                reqId,
                exchange,
                underlyingConId,
                tradingClass,
                multiplier,
                expirationsSize,
                rest: _*
              ) =>
            val expirationsSizeInt = expirationsSize.toInt
            val (expirations, stikes) = rest.toArray.splitAt(expirationsSizeInt)
            Right(
              SecurityDefinitionOptionalParameter(
                reqId.toInt,
                exchange,
                underlyingConId.toInt,
                tradingClass,
                multiplier,
                expirations.toSet,
                stikes.tail.toSet.map(_.toDouble)
              )
            )
        }
      )

  end given

  inline given Decoder[CommissionReportMsg] =
    summon[Decoder[CommissionReport]].map(CommissionReportMsg(_))

  inline given Decoder[VerifyAndAuthCompleted] =
    summon[Decoder[VerifyCompleted]].map(v =>
      VerifyAndAuthCompleted(v.isSuccessful, v.errorText)
    )
  end given

  inline given Decoder[VerifyCompleted] with
    def apply(
        entry: Array[String]
    ): Either[Throwable, VerifyCompleted] =
      partiallyApply(
        entry,
        { case Array(isSuccessfulStr, errorText) =>
          Right(VerifyCompleted("true" == isSuccessfulStr, errorText))
        }
      )
  end given

  inline given Decoder[DeltaNeutralValidation] with
    def apply(
        entry: Array[String]
    ): Either[Throwable, DeltaNeutralValidation] =
      partiallyApply(
        entry,
        { case Array(reqId, rest: _*) =>
          for
            reqIdInt <- summon[Decoder[Int]](Array(reqId))
            deltaNeutralContract <- summon[Decoder[DeltaNeutralContract]](
              rest.toArray
            )
          yield DeltaNeutralValidation(reqIdInt, deltaNeutralContract)
        }
      )
  end given

  inline given Decoder[HistoricalTicks] with
    @tailrec
    private def buildHistoricalTicks(
        entry: List[String],
        acc: List[HistoricalTick]
    ): List[HistoricalTick] =
      entry match
        case time :: _ :: price :: size :: xs =>
          buildHistoricalTicks(
            xs,
            HistoricalTick(
              time.toLong,
              price.toDouble,
              Decimal.parse(size).getOrElse(Decimal.INVALID)
            ) :: acc
          )
        case _ =>
          acc.reverse
      end match

    def apply(
        entry: Array[String]
    ): Either[Throwable, HistoricalTicks] =
      partiallyApply(
        entry,
        { case Array(reqId, tickCount, rest: _*) =>
          val entries = rest.toList
          Right(
            HistoricalTicks(
              reqId.toInt,
              buildHistoricalTicks(entries.init, Nil),
              entries.last.toBoolean
            )
          )
        }
      )
  end given

  inline given Decoder[HistoricalTicksBidAsk] with
    @tailrec
    private def buildHistoricalTickBidAsks(
        entry: List[String],
        acc: List[HistoricalTickBidAsk]
    ): List[HistoricalTickBidAsk] =
      entry match
        case time :: mask :: priceBid :: priceAsk :: sizeBid :: sizeAsk :: xs =>
          val bitMask = BitMask(mask.toInt)

          val tickAttribBidAsk = TickAttribBidAsk(
            askPastHigh = bitMask.get(0),
            bidPastLow = bitMask.get(1)
          )
          val tickBiAsk = HistoricalTickBidAsk(
            time = time.toLong,
            tickAttribBidAsk = tickAttribBidAsk,
            priceBid = priceBid.toDouble,
            priceAsk = priceAsk.toDouble,
            sizeBid = Decimal.parse(sizeBid).getOrElse(Decimal.INVALID),
            sizeAsk = Decimal.parse(sizeAsk).getOrElse(Decimal.INVALID)
          )
          buildHistoricalTickBidAsks(xs, tickBiAsk :: acc)
        case _ =>
          acc.reverse
      end match

    def apply(
        entry: Array[String]
    ): Either[Throwable, HistoricalTicksBidAsk] =
      partiallyApply(
        entry,
        { case Array(reqId, tickCount, rest: _*) =>
          val entries = rest.toList
          Right(
            HistoricalTicksBidAsk(
              reqId.toInt,
              buildHistoricalTickBidAsks(entries.init, Nil),
              entries.last.toBoolean
            )
          )
        }
      )
  end given

  inline given Decoder[HistoricalTicksLast] with
    @tailrec
    private def buildHistoricalTickLasts(
        entry: List[String],
        acc: List[HistoricalTickLast]
    ): List[HistoricalTickLast] =
      entry match
        case time :: mask :: price :: size :: exchange :: specialConditions :: xs =>
          val bitMask = BitMask(mask.toInt)

          val tickAttribLast = TickAttribLast(
            pastLimit = bitMask.get(0),
            unreported = bitMask.get(1)
          )
          val tickLast = HistoricalTickLast(
            time = time.toLong,
            tickAttribLast = tickAttribLast,
            price = price.toDouble,
            size = Decimal.parse(size).getOrElse(Decimal.INVALID),
            exchange = exchange,
            specialConditions = specialConditions
          )
          buildHistoricalTickLasts(xs, tickLast :: acc)
        case _ =>
          acc.reverse
      end match
    def apply(
        entry: Array[String]
    ): Either[Throwable, HistoricalTicksLast] =
      partiallyApply(
        entry,
        { case Array(reqId, tickCount, rest: _*) =>
          val entries = rest.toList
          Right(
            HistoricalTicksLast(
              reqId.toInt,
              buildHistoricalTickLasts(entries.init, Nil),
              entries.last.toBoolean
            )
          )
        }
      )
  end given

  // inline given Decoder[TickByTickAllLast] with
  //   def apply(
  //       entry: Array[String]
  //   ): Either[Throwable, TickByTickAllLast] =
  //     partiallyApply(
  //       entry,
  //       { case Array(reqId, rest: _*) =>
  //         for
  //           reqIdInt <- summon[Decoder[Int]](Array(reqId))
  //           deltaNeutralContract <- summon[Decoder[DeltaNeutralContract]](
  //             rest.toArray
  //           )
  //         yield DeltaNeutralValidation(reqIdInt, deltaNeutralContract)
  //       }
  //     )
  // end given

  inline given Decoder[HistoricalDataUpdate] with
    def apply(
        entry: Array[String]
    ): Either[Throwable, HistoricalDataUpdate] =
      partiallyApply(
        entry,
        { case Array(reqId, rest: _*) =>
          for
            reqIdInt <- summon[Decoder[Int]](Array(reqId))
            bar <- summon[Decoder[Bar]](
              rest.toArray
            )
          yield HistoricalDataUpdate(reqIdInt, bar)
        }
      )
  end given

  // inline given Decoder[UpdatePortfolio] with
  //   def apply(
  //       entry: Array[String]
  //   ): Either[Throwable, UpdatePortfolio] =
  //     partiallyApply(
  //       entry,
  //       { case Array(version, rest: _*) =>
  //         val versionInt = version.toInt
  //         val (conid, remaining) = if versionInt >= 6 then
  //           (rest.head.toInt, rest.tail)
  //         else (0, rest)
  //         val Array(symbol,secType,lastTradeDateOrContractMonth,strike,right, rest2:_*)  = remaining
  //         if versionInt >= 7 then
  //         else
  //       }
  //     )
  // end given

  inline given Decoder[HistoricalData] with
    @tailrec
    private def buildHistoricalData(
        reqId: Int,
        version: Int,
        entry: List[String],
        acc: List[HistoricalDataUpdate]
    ): List[HistoricalDataUpdate] =
      entry match
        case date :: open :: high :: low :: close :: volume :: wap :: xs =>
          buildHistoricalData(
            reqId,
            version,
            if version >= 3 then xs.tail else xs,
            HistoricalDataUpdate(
              reqId,
              Bar(
                count = if version >= 3 then xs.head.toInt else -1,
                time = date,
                open = open.toDouble,
                close = close.toDouble,
                high = high.toDouble,
                low = low.toDouble,
                wap = Decimal.parse(wap).getOrElse(Decimal.INVALID),
                volume = Decimal.parse(volume).getOrElse(Decimal.INVALID)
              )
            ) :: acc
          )
        case _ => acc.reverse

      end match
    def apply(
        entry: Array[String]
    ): Either[Throwable, HistoricalData] =
      partiallyApply(
        entry,
        { case Array(version, reqId, rest: _*) =>
          val versionInt = version.toInt
          val (startDate, endDate, data) =
            if (versionInt >= 2) then
              rest.toArray match
                case Array(star, end, remaining: _*) =>
                  (star, end, remaining)
                case _ => ("", "", rest)
            else ("", "", rest)
          Right(
            HistoricalData(
              buildHistoricalData(
                reqId.toInt,
                versionInt,
                data.toList.tail,
                Nil
              ),
              startDate,
              endDate
            )
          )
        }
      )
  end given

  inline given Decoder[HistoricalSchedule] with

    @tailrec
    private def buildHistoricalSessions(
        entry: List[String],
        acc: List[HistoricalSession]
    ): List[HistoricalSession] =
      entry match
        case startDateTime :: endDateTime :: refDate :: xs =>
          buildHistoricalSessions(
            xs,
            HistoricalSession(startDateTime, endDateTime, refDate) :: acc
          )
        case _ => acc.reverse

      end match

    def apply(
        entry: Array[String]
    ): Either[Throwable, HistoricalSchedule] =
      partiallyApply(
        entry,
        {
          case Array(
                reqId,
                startDateTime,
                endDateTime,
                timeZone,
                sessionsCount,
                sessions: _*
              ) =>
            Right(
              HistoricalSchedule(
                reqId = reqId.toInt,
                startDateTime = startDateTime,
                endDateTime = endDateTime,
                timeZone = timeZone,
                sessions = buildHistoricalSessions(sessions.toList, Nil)
              )
            )
        }
      )
  end given

  inline given Decoder[HistogramData] with

    @tailrec
    private def buildHistogramEntries(
        entry: List[String],
        acc: List[HistogramEntry]
    ): List[HistogramEntry] =
      entry match
        case price :: size :: xs =>
          buildHistogramEntries(
            xs,
            HistogramEntry(
              price.toDouble,
              Decimal.parse(size).getOrElse(Decimal.INVALID)
            ) :: acc
          )
        case _ => acc.reverse

      end match

    def apply(
        entry: Array[String]
    ): Either[Throwable, HistogramData] =
      partiallyApply(
        entry,
        {
          case Array(
                reqId,
                size,
                entries: _*
              ) =>
            Right(
              HistogramData(
                reqId = reqId.toInt,
                items = buildHistogramEntries(entries.toList, Nil)
              )
            )
        }
      )
  end given

  inline given Decoder[FamilyCodes] = new Decoder[FamilyCodes]:
    @tailrec
    private def buildFamilyCodes(
        entry: List[String],
        acc: List[FamilyCode]
    ): List[FamilyCode] =
      entry match
        case Nil      => acc.reverse
        case _ :: Nil => Nil
        case accountID :: familyCodeStr :: xs =>
          buildFamilyCodes(xs, FamilyCode(accountID, familyCodeStr) :: acc)
      end match

    override def apply(entry: Array[String]): Either[Throwable, FamilyCodes] =
      partiallyApply(
        entry,
        { case Array(numberOfFamilyCode, rest: _*) =>
          if numberOfFamilyCode == "1" then Right(FamilyCodes(Nil))
          else Right(FamilyCodes(buildFamilyCodes(rest.toList, Nil)))
        }
      )

  end given

  inline given Decoder[SmartComponents] with

    @tailrec
    private def buildSmartComponents(
        entry: List[String],
        acc: Map[Int, (String, Char)]
    ): Map[Int, (String, Char)] =
      entry match
        case bitNumber :: exchange :: exchangeLetter :: xs =>
          buildSmartComponents(
            xs,
            acc + (bitNumber.toInt -> (exchange -> exchangeLetter.head))
          )
        case _ => acc
      end match

    def apply(
        entry: Array[String]
    ): Either[Throwable, SmartComponents] =
      partiallyApply(
        entry,
        {
          case Array(
                reqId,
                size,
                rest: _*
              ) =>
            val intDecoder = summon[Decoder[Int]]
            Right(
              SmartComponents(
                reqId = reqId.toInt,
                theMap = buildSmartComponents(rest.toList, Map.empty)
              )
            )
        }
      )
  end given

  inline given Decoder[MktDepthExchanges] = new Decoder[MktDepthExchanges]:
    @tailrec
    private def buildMktDepthExchanges(
        entry: List[String],
        acc: List[DepthMktDataDescription]
    ): List[DepthMktDataDescription] =
      entry match
        case exchange :: secType :: listingExch :: serviceDataType :: aggGroup :: xs =>
          buildMktDepthExchanges(
            xs,
            DepthMktDataDescription(
              exchange,
              SecType.valueOf(secType),
              listingExch,
              serviceDataType,
              aggGroup.toInt
            ) :: acc
          )
        case _ => acc.reverse
      end match

    override def apply(
        entry: Array[String]
    ): Either[Throwable, MktDepthExchanges] =
      partiallyApply(
        entry,
        { case Array(nDepthMktDataDescriptions, rest: _*) =>
          Right(MktDepthExchanges(buildMktDepthExchanges(rest.toList, Nil)))
        }
      )

  end given

  inline given Decoder[NewsProviders] = new Decoder[NewsProviders]:
    @tailrec
    private def buildNewsProviders(
        entry: List[String],
        acc: List[NewsProvider]
    ): List[NewsProvider] =
      entry match
        case providerCode :: providerName :: xs =>
          buildNewsProviders(
            xs,
            NewsProvider(
              providerCode,
              providerName
            ) :: acc
          )
        case _ => acc.reverse
      end match

    override def apply(
        entry: Array[String]
    ): Either[Throwable, NewsProviders] =
      partiallyApply(
        entry,
        { case Array(nNewsProviders, rest: _*) =>
          Right(NewsProviders(buildNewsProviders(rest.toList, Nil)))
        }
      )

  end given

  inline given Decoder[SoftDollarTiers] = new Decoder[SoftDollarTiers]:
    @tailrec
    private def buildSoftDollarTiers(
        entry: List[String],
        acc: List[SoftDollarTier]
    ): List[SoftDollarTier] =
      entry match
        case name :: value :: displayName :: xs =>
          buildSoftDollarTiers(
            xs,
            SoftDollarTier(
              name,
              value,
              displayName
            ) :: acc
          )
        case _ => acc.reverse
      end match

    override def apply(
        entry: Array[String]
    ): Either[Throwable, SoftDollarTiers] =
      partiallyApply(
        entry,
        { case Array(reqId, nTiers, rest: _*) =>
          Right(
            SoftDollarTiers(reqId.toInt, buildSoftDollarTiers(rest.toList, Nil))
          )
        }
      )

  end given

  inline given Decoder[MarketRule] = new Decoder[MarketRule]:
    @tailrec
    private def buildMarketRule(
        entry: List[String],
        acc: List[PriceIncrement]
    ): List[PriceIncrement] =
      entry match
        case lowEdge :: increment :: xs =>
          buildMarketRule(
            xs,
            PriceIncrement(
              lowEdge.toDouble,
              increment.toDouble
            ) :: acc
          )
        case _ => acc.reverse
      end match

    override def apply(
        entry: Array[String]
    ): Either[Throwable, MarketRule] =
      partiallyApply(
        entry,
        { case Array(marketRuleId, nPriceIncrements, rest: _*) =>
          Right(
            MarketRule(marketRuleId.toInt, buildMarketRule(rest.toList, Nil))
          )
        }
      )

  end given

  inline given Decoder[ResponseMsg] with
    def apply(entry: Array[String]): Either[Throwable, ResponseMsg] =
      partiallyApply(
        entry,
        { case Array(msgId, rest: _*) =>
          val msg = rest.toArray
          msgId.toInt match
            case END_CONN =>
              Right(ConnectionClosed)
            case TICK_PRICE =>
              Decoder.decode[TickPrice](msg)
            case TICK_SIZE =>
              Decoder.decode[TickSize](msg.tail)
            case POSITION =>
              Decoder.decode[PositionMsg](msg)
            case POSITION_END =>
              Right(PositionEnd)
            case ACCOUNT_SUMMARY =>
              Decoder.decode[AccountSummary](msg.tail)
            case ACCOUNT_SUMMARY_END =>
              Decoder.decode[AccountSummaryEnd](msg.tail)
            case TICK_OPTION_COMPUTATION =>
              Decoder.decode[TickOptionComputation](msg)
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
            case PORTFOLIO_VALUE =>
              Decoder.decode[UpdatePortfolio](msg)
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
            case HISTORICAL_DATA =>
              Decoder.decode[HistoricalData](msg)
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
            case HISTOGRAM_DATA =>
              Decoder.decode[HistogramData](msg)
            case HISTORICAL_DATA_UPDATE =>
              Decoder.decode[HistoricalDataUpdate](msg)
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
            case HISTORICAL_TICKS =>
              Decoder.decode[HistoricalTicks](msg)
            case HISTORICAL_TICKS_BID_ASK =>
              Decoder.decode[HistoricalTicksBidAsk](msg)
            case HISTORICAL_TICKS_LAST =>
              Decoder.decode[HistoricalTicksLast](msg)
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
              Decoder.decode[WshMetaDataMsg](msg)
            case WSH_EVENT_DATA =>
              Decoder.decode[WshEventDataMsg](msg)
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
      )
  end given
