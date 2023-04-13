package org.quasigroup.ibclient.client.response

import org.quasigroup.ibclient.client.types.*
import org.quasigroup.ibclient.client.decoder.Decoder
import org.quasigroup.ibclient.client.decoder.Decoder.*
import org.quasigroup.ibclient.client.response.ResponseMsg.*

import cats.syntax.option._
import scala.annotation.tailrec

object MsgDecoders:
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

  inline given Decoder[Position] with
    def apply(
        entry: Array[String]
    ): Either[Throwable, Position] =
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
              right,
              multiplier,
              exchange,
              currency,
              localSymbol,
              tradingClass
            )
            Right(Position(account, contract, pos, avgCost))
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
              right,
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
        { case Array(reqId, conid, delta, price) =>
          Right(
            DeltaNeutralValidation(
              reqId.toInt,
              DeltaNeutralContract(conid.toInt, delta.toDouble, price.toDouble)
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
            val intDecoder = summon[Decoder[Int]]
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
