package org.quasigroup.ibclient.types

import org.quasigroup.ibclient.encoder.Encoder.{*, given}
import org.quasigroup.ibclient.decoder.Decoder.{*, given}
import org.quasigroup.ibclient.decoder.Decoder
import org.quasigroup.ibclient.encoder.Encoder

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import scala.collection.mutable

object TypesCodec:
  def writeModifiedUTF(str: String): mutable.Buffer[Byte] = {
    val byteArrayStream = new ByteArrayOutputStream()
    val objectOutput = new ObjectOutputStream(byteArrayStream)
    objectOutput.writeUTF(str)
    mutable.Buffer.from(byteArrayStream.toByteArray)
  }
  inline given Encoder[Decimal] =
    summon[Encoder[String]].contramap(decimal =>
      if decimal == Decimal.INVALID then ""
      else decimal.value.bigDecimal.stripTrailingZeros.toPlainString
    )

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
    summon[Encoder[String]].contramap(method =>
      if method == Method.Ignored then "" else method.toString
    )

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

  inline given Encoder[Contract] with
    override def apply(contract: Contract): mutable.Buffer[Byte] =
      summon[Encoder[Int]](contract.conId)
        ++ summon[Encoder[String]](contract.symbol)
        ++ summon[Encoder[SecType]](contract.secType)
        ++ summon[Encoder[String]](contract.lastTradeDateOrContractMonth)
        ++ summon[Encoder[Double]](contract.strike)
        ++ summon[Encoder[ContractRight]](contract.right)
        ++ summon[Encoder[String]](contract.multiplier)
        ++ summon[Encoder[String]](contract.exchange)
        ++ summon[Encoder[String]](contract.primaryExch)
        ++ summon[Encoder[String]](contract.currency)
        ++ summon[Encoder[String]](contract.localSymbol)
        ++ summon[Encoder[String]](contract.tradingClass)
        ++ summon[Encoder[Boolean]](contract.includeExpired)

//     // override def apply(x: ContractRight): mutable.Buffer[Byte] =
//     //   val str =
//     //     x match
//     //       case ContractRight.Ignored => ""
//     //       case ContractRight.Put     => "P"
//     //       case ContractRight.Call    => "C"
//     //   end match
//     //   summon[Encoder[String]](x)
//   end given
