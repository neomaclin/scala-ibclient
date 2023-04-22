package org.quasigroup.ibclient.response

import MsgId.*
import org.quasigroup.ibclient.exceptions.EClientErrors
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.{*, given}

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

  inline given Decoder[UpdatePortfolio] with
    private val createUpdatePortfolio: DecoderState[UpdatePortfolio] =
      for
        version <- read[Int]
        conId <- if version >= 6 then read[Int] else readNothing(0)
        symbol <- read[String]
        secType <- read[SecType]
        lastTradeDateOrContractMonth <- read[String]
        strike <- read[Double]
        right <- read[ContractRight]
        multiplier <-
          if version >= 7 then read[String] else readNothing("")
        primaryExch <-
          if version >= 7 then read[String] else readNothing("")
        currency <- read[String]
        localSymbol <-
          if version >= 2 then read[String] else readNothing("")
        tradingClass <-
          if version >= 8 then read[String] else readNothing("")
        position <- read[Decimal]
        marketPrice <- read[Double]
        marketValue <- read[Double]
        averageCost <-
          if version >= 3 then read[Double] else readNothing(0.0)
        unrealizedPNL <-
          if version >= 3 then read[Double] else readNothing(0.0)
        realizedPNL <-
          if version >= 3 then read[Double] else readNothing(0.0)
        accountName <-
          if version >= 4 then read[String] else readNothing("")
      yield
        val contract = Contract(
          conId = conId,
          symbol = symbol,
          secType = secType,
          lastTradeDateOrContractMonth = lastTradeDateOrContractMonth,
          strike = strike,
          right = right,
          multiplier = multiplier,
          primaryExch = primaryExch,
          currency = currency,
          localSymbol = localSymbol,
          tradingClass = tradingClass
        )
        UpdatePortfolio(
          contract,
          position,
          marketPrice,
          marketValue,
          averageCost,
          unrealizedPNL,
          realizedPNL,
          accountName
        )
    override def apply(
        entry: Array[String]
    ): Either[Throwable, UpdatePortfolio] =
      createUpdatePortfolio.runA(entry)

  inline given Decoder[TickPrice] with
    private val createTickPice: DecoderState[TickPrice] =
      for
        version <- read[Int]
        tickerId <- read[Int]
        tickType <- read[TickType]
        price <- read[Double]
        size <-
          if version >= 2 then read[Decimal] else readNothing(Decimal.INVALID)
        tickAttrib <-
          if version >= 3 then
            read[Int].map { attrMask =>
              val mask = BitMask(attrMask)
              TickAttrib(
                canAutoExecute = mask.get(0),
                pastLimit = mask.get(1),
                preOpen = mask.get(2)
              )
            }
          else readNothing(TickAttrib())
      yield {
        val optionalSizeTickType =
          if version >= 2 then
            tickType match
              case TickType.BID          => TickType.BID_SIZE.some
              case TickType.ASK          => TickType.ASK_SIZE.some
              case TickType.LAST         => TickType.LAST_SIZE.some
              case TickType.DELAYED_BID  => TickType.DELAYED_BID_SIZE.some
              case TickType.DELAYED_ASK  => TickType.DELAYED_ASK_SIZE.some
              case TickType.DELAYED_LAST => TickType.DELAYED_LAST_SIZE.some
              case _                     => None
          else None
        TickPrice(
          tickerId,
          tickType,
          price,
          tickAttrib,
          optionalSizeTickType.map(TickSize(tickerId, _, size))
        )
      }
    override def apply(
        entry: Array[String]
    ): Either[Throwable, TickPrice] = createTickPice.runA(entry)
  end given

  // inline given Decoder[TickOptionComputation] with
  //   private val createTickOptionComputation =

  //   def apply(
  //       entry: Array[String]
  //   ): Either[Throwable, TickOptionComputation] =
  //     // partiallyApply(
  //     //   entry,
  //     //   {
  //     //     case Array(
  //     //           version,
  //     //           tickerId,
  //     //           tickType,
  //     //           impliedVol,
  //     //           delta,
  //     //           rest: _*
  //     //         ) =>
  //     //       val versionInt = version.toInt
  //     //       val tickerIdInt = tickerId.toInt
  //     //       val tickTypeEnum = TickType.fromOrdinal(tickType.toInt)
  //     //       val impliedVolDouble = Try(impliedVol.toDouble)
  //     //         .filter(_ != -1)
  //     //         .getOrElse(Double.MaxValue)
  //     //       val deltaDouble =
  //     //         Try(delta.toDouble).filter(_ != -2).getOrElse(Double.MaxValue)
  //     //       val (optPriceDouble, pvDividendDouble, otherRest) =
  //     //         if (
  //     //           versionInt >= 6 || tickTypeEnum == TickType.MODEL_OPTION || tickTypeEnum == TickType.DELAYED_MODEL_OPTION
  //     //         )
  //     //         then
  //     //           rest.toArray match
  //     //             case Array(optPrice, pvDividend, remaining: _*) =>
  //     //               val optPriceDouble = Try(optPrice.toDouble)
  //     //                 .filter(_ != -1)
  //     //                 .getOrElse(Double.MaxValue)
  //     //               val pvDividendDouble = Try(pvDividend.toDouble)
  //     //                 .filter(_ != -1)
  //     //                 .getOrElse(Double.MaxValue)
  //     //               (optPriceDouble, pvDividendDouble, remaining)
  //     //             case _ => (Double.MaxValue, Double.MaxValue, rest)
  //     //         else (Double.MaxValue, Double.MaxValue, rest)
  //     //       val (gammaDouble, vegaDouble, thetaDouble, undPriceDouble) =
  //     //         if (versionInt >= 6) then
  //     //           rest.toArray match
  //     //             case Array(gamma, vega, theta, undPrice) =>
  //     //               val gammaDouble = Try(gamma.toDouble)
  //     //                 .filter(_ != -2)
  //     //                 .getOrElse(Double.MaxValue)
  //     //               val vegaDouble = Try(vega.toDouble)
  //     //                 .filter(_ != -2)
  //     //                 .getOrElse(Double.MaxValue)
  //     //               val thetaDouble = Try(theta.toDouble)
  //     //                 .filter(_ != -2)
  //     //                 .getOrElse(Double.MaxValue)
  //     //               val undPriceDouble = Try(undPrice.toDouble)
  //     //                 .filter(_ != -1)
  //     //                 .getOrElse(Double.MaxValue)
  //     //               (gammaDouble, vegaDouble, thetaDouble, undPriceDouble)
  //     //             case _ =>
  //     //               (
  //     //                 Double.MaxValue,
  //     //                 Double.MaxValue,
  //     //                 Double.MaxValue,
  //     //                 Double.MaxValue
  //     //               )
  //     //         else
  //     //           (
  //     //             Double.MaxValue,
  //     //             Double.MaxValue,
  //     //             Double.MaxValue,
  //     //             Double.MaxValue
  //     //           )
  //     //       Right(
  //     //         TickOptionComputation(
  //     //           tickerIdInt,
  //     //           tickTypeEnum,
  //     //           impliedVolDouble,
  //     //           deltaDouble,
  //     //           optPriceDouble,
  //     //           pvDividendDouble,
  //     //           gammaDouble,
  //     //           vegaDouble,
  //     //           thetaDouble,
  //     //           undPriceDouble
  //     //         )
  //     //       )

  //     //   }
  //     // )

  // end given

  inline given Decoder[PositionMsg] with
    private val createPositionMsg: DecoderState[PositionMsg] =
      for
        version <- read[Int]
        account <- read[String]
        conId <- read[Int]
        symbol <- read[String]
        secType <- read[SecType]
        lastTradeDateOrContractMonth <- read[String]
        strike <- read[Double]
        right <- read[ContractRight]
        multiplier <- read[String]
        exchange <- read[String]
        currency <- read[String]
        localSymbol <- read[String]
        tradingClass <-
          if version >= 2 then read[String] else readNothing("")
        pos <- read[Decimal]
        avgCost <- if version >= 3 then read[Double] else readNothing(0.0)
      yield PositionMsg(
        account,
        Contract(
          conId,
          symbol,
          secType,
          lastTradeDateOrContractMonth,
          strike,
          right,
          multiplier,
          exchange,
          currency,
          localSymbol,
          tradingClass
        ),
        pos,
        avgCost
      )

    override def apply(
        entry: Array[String]
    ): Either[Throwable, PositionMsg] = createPositionMsg.runA(entry)
  end given

  inline given Decoder[PositionMulti] with
    private val createPositionMulti: DecoderState[PositionMulti] =
      for
        reqId <- read[Int]
        account <- read[String]
        conId <- read[Int]
        symbol <- read[String]
        secType <- read[SecType]
        lastTradeDateOrContractMonth <- read[String]
        strike <- read[Double]
        right <- read[ContractRight]
        multiplier <- read[String]
        exchange <- read[String]
        currency <- read[String]
        localSymbol <- read[String]
        tradingClass <- read[String]
        pos <- read[Decimal]
        avgCost <- read[Double]
        modelCode <- read[String]
      yield PositionMulti(
        reqId,
        account,
        modelCode,
        Contract(
          conId,
          symbol,
          secType,
          lastTradeDateOrContractMonth,
          strike,
          right,
          multiplier,
          exchange,
          currency,
          localSymbol,
          tradingClass
        ),
        pos,
        avgCost
      )

    override def apply(
        entry: Array[String]
    ): Either[Throwable, PositionMulti] = createPositionMulti.runA(entry)
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
    private val createVerifyCompleted: DecoderState[VerifyCompleted] =
      for
        isSuccessfulStr <- read[String]
        errorText <- read[String]
      yield VerifyCompleted("true" == isSuccessfulStr, errorText)

    override def apply(
        entry: Array[String]
    ): Either[Throwable, VerifyCompleted] = createVerifyCompleted.runA(entry)

  end given

  inline given Decoder[DeltaNeutralValidation] with
    private val createDeltaNeutralValidation
        : DecoderState[DeltaNeutralValidation] =
      for
        reqId <- read[Int]
        conid <- read[Int]
        delta <- read[Double]
        price <- read[Double]
      yield DeltaNeutralValidation(
        reqId,
        DeltaNeutralContract(conid, delta, price)
      )

    def apply(
        entry: Array[String]
    ): Either[Throwable, DeltaNeutralValidation] =
      createDeltaNeutralValidation.runA(entry)
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

  inline given Decoder[HistoricalData] with
    private val createHistoricalData: DecoderState[HistoricalData] =
      for
        version <- read[Int]
        reqId <- read[Int]
        startDate <- if (version >= 2) then read[String] else readNothing("")
        endDate <- if (version >= 2) then read[String] else readNothing("")
        itemCount <- read[Int]
        histories <-
          (0 until itemCount).foldLeft(
            readNothing(List.empty[HistoricalDataUpdate])
          ) { (state, idx) =>
            for
              list <- state
              date <- read[String]
              open <- read[Double]
              high <- read[Double]
              low <- read[Double]
              close <- read[Double]
              volume <- read[Decimal]
              wap <- read[Decimal]
              barCount <- if version >= 3 then read[Int] else readNothing(-1)
            yield HistoricalDataUpdate(
              reqId,
              Bar(
                time = date,
                open = open,
                high = high,
                low = low,
                close = close,
                volume = volume,
                count = barCount,
                wap = wap
              )
            ) :: list
          }
      yield HistoricalData(histories.reverse, startDate, endDate)
    override def apply(
        entry: Array[String]
    ): Either[Throwable, HistoricalData] =
      createHistoricalData.runA(entry)
  end given

  inline given Decoder[HistoricalSchedule] with
    private val createHistoricalSchedule: DecoderState[HistoricalSchedule] =
      for
        reqId <- read[Int]
        startDateTime <- read[String]
        endDateTime <- read[String]
        timeZone <- read[String]
        sessionsCount <- read[Int]
        sessions <-
          (0 until sessionsCount).foldLeft(
            readNothing(List.empty[HistoricalSession])
          ) { (state, idx) =>
            for
              list <- state
              sessionStartDateTime <- read[String]
              sessionEndDateTime <- read[String]
              sessionRefDateTime <- read[String]
            yield HistoricalSession(
              sessionStartDateTime,
              sessionEndDateTime,
              sessionRefDateTime
            ) :: list
          }
      yield HistoricalSchedule(
        reqId,
        startDateTime,
        endDateTime,
        timeZone,
        sessions.reverse
      )

    override def apply(
        entry: Array[String]
    ): Either[Throwable, HistoricalSchedule] =
      createHistoricalSchedule.runA(entry)
  end given

  inline given Decoder[HistogramData] with
    private val createHistogramData: DecoderState[HistogramData] =
      for
        reqId <- read[Int]
        nEntries <- read[Int]
        entries <-
          (0 until nEntries).foldLeft(
            readNothing(List.empty[HistogramEntry])
          ) { (state, idx) =>
            for
              list <- state
              price <- read[Double]
              size <- read[Decimal]
            yield HistogramEntry(price, size) :: list
          }
      yield HistogramData(reqId, entries.reverse)

    override def apply(
        entry: Array[String]
    ): Either[Throwable, HistogramData] = createHistogramData.runA(entry)

  end given

  inline given Decoder[FamilyCodes] with
    private val createFamilyCodes: DecoderState[FamilyCodes] =
      for
        numberOfFamilyCode <- read[Int]
        familyCodes <-
          if numberOfFamilyCode == 1 then readNothing(List.empty[FamilyCode])
          else
            (0 until numberOfFamilyCode).foldLeft(
              readNothing(List.empty[FamilyCode])
            ) { (state, idx) =>
              for
                list <- state
                accountID <- read[String]
                familyCodeStr <- read[String]
              yield FamilyCode(accountID, familyCodeStr) :: list
            }
      yield FamilyCodes(familyCodes.reverse)

    override def apply(
        entry: Array[String]
    ): Either[Throwable, FamilyCodes] = createFamilyCodes.runA(entry)

  end given

  inline given Decoder[SmartComponents] with
    private val createSmartComponents: DecoderState[SmartComponents] =
      for
        reqId <- read[Int]
        size <- read[Int]
        smartComponents <-
          (0 until size).foldLeft(
            readNothing(Map.empty[Int, (String, Char)])
          ) { (state, idx) =>
            for
              maps <- state
              bitNumber <- read[Int]
              exchange <- read[String]
              exchangeLetter <- read[String]
            yield maps + (bitNumber.toInt -> (exchange -> exchangeLetter.head))
          }
      yield SmartComponents(reqId, smartComponents)

    override def apply(
        entry: Array[String]
    ): Either[Throwable, SmartComponents] = createSmartComponents.runA(entry)

  end given

  inline given Decoder[MktDepthExchanges] with
    private val createMktDepthExchanges: DecoderState[MktDepthExchanges] =
      for
        nDepthMktDataDescriptions <- read[Int]
        depthMktDataDescriptions <-
          (0 until nDepthMktDataDescriptions).foldLeft(
            readNothing(List.empty[DepthMktDataDescription])
          ) { (state, idx) =>
            for
              list <- state
              exchange <- read[String]
              secType <- read[SecType]
              listingExch <- read[String]
              serviceDataType <- read[String]
              aggGroup <- read[Int]
            yield DepthMktDataDescription(
              exchange,
              secType,
              listingExch,
              serviceDataType,
              aggGroup
            ) :: list
          }
      yield MktDepthExchanges(depthMktDataDescriptions.reverse)

    override def apply(
        entry: Array[String]
    ): Either[Throwable, MktDepthExchanges] =
      createMktDepthExchanges.runA(entry)

  end given

  inline given Decoder[NewsProviders] with
    private val createNewsProviders: DecoderState[NewsProviders] =
      for
        nNewsProviders <- read[Int]
        newsProviders <-
          (0 until nNewsProviders).foldLeft(
            readNothing(List.empty[NewsProvider])
          ) { (state, idx) =>
            for
              list <- state
              providerCode <- read[String]
              providerName <- read[String]
            yield NewsProvider(providerCode, providerName) :: list
          }
      yield NewsProviders(newsProviders.reverse)

    override def apply(entry: Array[String]): Either[Throwable, NewsProviders] =
      createNewsProviders.runA(entry)

  end given

  inline given Decoder[SoftDollarTiers] with
    private val createSoftDollarTiers: DecoderState[SoftDollarTiers] =
      for
        reqId <- read[Int]
        nTiers <- read[Int]
        tiers <-
          (0 until nTiers).foldLeft(readNothing(List.empty[SoftDollarTier])) {
            (state, idx) =>
              for
                list <- state
                name <- read[String]
                value <- read[String]
                displayName <- read[String]
              yield SoftDollarTier(name, value, displayName) :: list
          }
      yield SoftDollarTiers(reqId, tiers.reverse)

    override def apply(
        entry: Array[String]
    ): Either[Throwable, SoftDollarTiers] = createSoftDollarTiers.runA(entry)

  end given

  inline given Decoder[MarketRule] with
    private val createMarketRule: DecoderState[MarketRule] =
      for
        marketRuleId <- read[Int]
        nPriceIncrements <- read[Int]
        priceIncrements <-
          (0 until nPriceIncrements).foldLeft(
            readNothing(List.empty[PriceIncrement])
          ) { (state, idx) =>
            for
              list <- state
              lowEdge <- read[Double]
              increment <- read[Double]
            yield PriceIncrement(lowEdge, increment) :: list
          }
      yield MarketRule(marketRuleId, priceIncrements.reverse)

    override def apply(entry: Array[String]): Either[Throwable, MarketRule] =
      createMarketRule.runA(entry)
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
