package org.quasigroup.ibclient.types

import org.quasigroup.ibclient.encoder.Encoder.{*, given}
import org.quasigroup.ibclient.decoder.Decoder.{*, given}
import org.quasigroup.ibclient.decoder.Decoder
import org.quasigroup.ibclient.encoder.Encoder

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import scala.collection.mutable

object TypesCodec:

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

  def writeModifiedUTF(str: String): Array[Byte] = {
    val byteArrayStream = new ByteArrayOutputStream()
    val objectOutput = new ObjectOutputStream(byteArrayStream)
    objectOutput.writeUTF(str)
    byteArrayStream.toByteArray
  }

  inline given Decoder[MktDataType] =
    summon[Decoder[Int]].map(MktDataType.fromOrdinal)

  inline given Decoder[NewsType] =
    summon[Decoder[Int]].map(NewsType.fromOrdinal)

  inline given Encoder[UsePriceMgmtAlgo] =
    summon[Encoder[Int]].contramap(_.value)

  inline given Encoder[VolatilityType] =
    summon[Encoder[Int]].contramap(volatilityType =>
      if volatilityType == VolatilityType.Ignored then Int.MaxValue
      else volatilityType.ordinal
    )

  inline given Encoder[AlgoStrategy] =
    summon[Encoder[String]].contramap(_.toString)

  inline given Encoder[HedgeType] =
    summon[Encoder[String]].contramap(hedgeType =>
      if hedgeType == HedgeType.Ignored then ""
      else hedgeType.toString.head.toString
    )

  inline given Encoder[Method] =
    summon[Encoder[String]].contramap(method => if method == Method.Ignored then "" else method.toString)

  inline given Encoder[TriggerMethod] =
    summon[Encoder[Int]].contramap(_.value)

  inline given Encoder[TimeInForce] =
    summon[Encoder[String]].contramap(_.toString)

  inline given Encoder[Order.Type] =
    summon[Encoder[String]].contramap(_.apiString)

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
          case ContractRight.Ignored => ""
          case ContractRight.Put     => "P"
          case ContractRight.Call    => "C"
      summon[Encoder[String]](str)
  end given

  inline given Encoder[Action] =
    summon[Encoder[String]].contramap(_.toString)

  inline given Encoder[TagValue] =
    summon[Encoder[String]].contramap(tag => tag.tag + "=" + tag.value + ";")

  inline given Decoder[ContractRight] =
    summon[Decoder[String]].map(ContractRight.fromString)
