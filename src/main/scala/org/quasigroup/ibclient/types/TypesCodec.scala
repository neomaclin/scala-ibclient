package org.quasigroup.ibclient.types

import org.quasigroup.ibclient.encoder.Encoder.{*, given}
import org.quasigroup.ibclient.decoder.Decoder.{*, given}
import org.quasigroup.ibclient.decoder.Decoder
import org.quasigroup.ibclient.encoder.Encoder

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import scala.collection.mutable

object TypesCodec:

  inline given Encoder[ReferencePriceType] = summon[Encoder[Int]].contramap(_.ordinal)

  inline given Decoder[ReferencePriceType] = summon[Decoder[Int]].map(ReferencePriceType.fromOrdinal)

  inline given Encoder[OcaType] = summon[Encoder[Int]].contramap(_.ordinal)

  inline given Decoder[OcaType] = summon[Decoder[Int]].map(OcaType.fromOrdinal)

  inline given Encoder[OrderConditionType] = summon[Encoder[Int]].contramap(_.value)

  inline given Decoder[OrderConditionType] = summon[Decoder[Int]].map(value =>
    OrderConditionType.values.find(_.value == value).getOrElse(OrderConditionType.Unknown)
  )

  inline given Decoder[TickAttrib] = summon[Decoder[Int]]
    .map(BitMask(_))
    .map(mask =>
      TickAttrib(
        canAutoExecute = mask.get(0),
        pastLimit = mask.get(1),
        preOpen = mask.get(2)
      )
    )

  inline given Decoder[TickAttribBidAsk] = summon[Decoder[Int]]
    .map(BitMask(_))
    .map(mask =>
      TickAttribBidAsk(
        askPastHigh = mask.get(0),
        bidPastLow = mask.get(1)
      )
    )

  inline given Decoder[TickAttribLast] = summon[Decoder[Int]]
    .map(BitMask(_))
    .map(mask =>
      TickAttribLast(
        pastLimit = mask.get(0),
        unreported = mask.get(1)
      )
    )

  inline given Decoder[Liquidities] = summon[Decoder[Int]].map(Liquidities.fromOrdinal)

  inline given Decoder[MktDataType] =
    summon[Decoder[Int]].map(MktDataType.fromOrdinal)

  inline given Decoder[NewsType] =
    summon[Decoder[Int]].map(NewsType.fromOrdinal)

  inline given Encoder[UsePriceMgmtAlgo] =
    summon[Encoder[String]].contramap(_.value)

  inline given Decoder[UsePriceMgmtAlgo] =
    summon[Decoder[String]].map(str => UsePriceMgmtAlgo.values.find(_.value == str).getOrElse(UsePriceMgmtAlgo.None))

  inline given Encoder[VolatilityType] =
    summon[Encoder[Int]].contramap(volatilityType =>
      if volatilityType == VolatilityType.None then Int.MaxValue
      else volatilityType.ordinal
    )

  inline given Decoder[VolatilityType] =
    summon[Decoder[Int]].map(int =>
      if int == Int.MaxValue then VolatilityType.None
      else VolatilityType.fromOrdinal(int)
    )

  inline given Encoder[AlgoStrategy] =
    summon[Encoder[String]].contramap(algoStrategy =>
      if algoStrategy == AlgoStrategy.None then ""
      else algoStrategy.toString
    )

  inline given Decoder[AlgoStrategy] =
    summon[Decoder[String]].map(str => if str.isEmpty then AlgoStrategy.None else AlgoStrategy.valueOf(str))

  inline given Encoder[HedgeType] =
    summon[Encoder[String]].contramap(hedgeType =>
      if hedgeType == HedgeType.None then ""
      else hedgeType.toString.head.toString
    )

  inline given Decoder[HedgeType] =
    summon[Decoder[String]].map(str => if str.isEmpty then HedgeType.None else HedgeType.valueOf(str))

  inline given Decoder[Method] =
    summon[Decoder[String]].map(value => Method.values.find(_.toString == value).getOrElse(Method.None))

  inline given Encoder[Method] =
    summon[Encoder[String]].contramap(method => if method == Method.None then "" else method.toString)

  inline given Encoder[TriggerMethod] =
    summon[Encoder[Int]].contramap(_.value)

  inline given Decoder[TriggerMethod] =
    summon[Decoder[Int]].map(value => TriggerMethod.values.find(_.value == value).getOrElse(TriggerMethod.Default))

  inline given Encoder[TimeInForce] =
    summon[Encoder[String]].contramap(_.toString)

  inline given Decoder[TimeInForce] =
    summon[Decoder[String]].map(TimeInForce.valueOf)

  inline given Encoder[Order.Type] =
    summon[Encoder[String]].contramap(_.apiString)

  inline given Decoder[Order.Type] =
    summon[Decoder[String]].flatMap(str =>
      Order.Type.values.find(_.apiString == str).toRight(new Exception("unknown order type"))
    )

  inline given Decoder[Order.Status] =
    summon[Decoder[String]].map(str => Order.Status.values.find(_.toString == str).getOrElse(Order.Status.Unknown))

  inline given Encoder[TickType] = summon[Encoder[Int]].contramap(_.index)

  inline given Decoder[TickType] =
    summon[Decoder[Int]].map(TickType.fromOrdinal)

  inline given Decoder[SecType] =
    summon[Decoder[String]].map(SecType.valueOf)

  inline given Encoder[SecType] =
    summon[Encoder[String]].contramap(_.toString)

  inline given Decoder[SecIdType] =
    summon[Decoder[String]].map(SecIdType.valueOf)

  inline given Encoder[SecIdType] =
    summon[Encoder[String]].contramap(_.toString)

  inline given Encoder[ContractRight] with
    override def apply(x: ContractRight): mutable.Buffer[Byte] =
      val str =
        x match
          case ContractRight.None => ""
          case ContractRight.Put  => "P"
          case ContractRight.Call => "C"
      summon[Encoder[String]](str)
  end given

  inline given Encoder[Action] =
    summon[Encoder[String]].contramap(_.toString)

  inline given Decoder[Action] =
    summon[Decoder[String]].map(Action.valueOf)

  inline given Encoder[AuctionStrategy] =
    summon[Encoder[Int]].contramap(_.ordinal)

  inline given Decoder[AuctionStrategy] =
    summon[Decoder[Int]].map(AuctionStrategy.fromOrdinal)

  inline given Encoder[Rule80A] =
    summon[Encoder[String]].contramap(_.value)

  inline given Decoder[Rule80A] =
    summon[Decoder[String]].flatMap(str =>
      Rule80A.values.find(_.value == str).toRight(new Exception("unknown Rule80A type"))
    )

  inline given Encoder[TagValue] =
    summon[Encoder[String]].contramap(tag => tag.tag + "=" + tag.value + ";")

  inline given Decoder[ContractRight] =
    summon[Decoder[String]].map(ContractRight.fromString)

  inline given Decoder[ComboLeg.OpenClose] =
    summon[Decoder[Int]].map(ComboLeg.OpenClose.fromOrdinal)
