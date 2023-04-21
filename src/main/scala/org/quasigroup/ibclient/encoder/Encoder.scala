package org.quasigroup.ibclient.encoder

import org.quasigroup.ibclient.types.Decimal
import org.quasigroup.ibclient.types.value
import fs2.Chunk
import scodec.bits.Literals.Utf8

import scala.collection.mutable
import scala.compiletime.summonFrom
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}

object Encoder:

  inline given Encoder[String] = _.getBytes.toBuffer.+=(0)

  inline given Encoder[Int] = summon[Encoder[String]].contramap(int =>
    if int == Int.MaxValue then "" else String.valueOf(int)
  )

  inline given Encoder[Array[Byte]] = _.toBuffer

  inline given Encoder[Boolean] =
    summon[Encoder[Int]].contramap(if _ then 1 else 0)

  inline given Encoder[Double] = summon[Encoder[String]].contramap(double =>
    if double == Double.MaxValue then "" else String.valueOf(double)
  )

  
  inline given Encoder[Decimal] =
    summon[Encoder[String]].contramap(decimal =>
      if decimal == Decimal.INVALID then ""
      else decimal.value.bigDecimal.stripTrailingZeros.toPlainString
    )


  inline def encoderSimplySum[T](
      s: Mirror.SumOf[T]
  ): Encoder[T] = new Encoder[T]:
    def apply(t: T): mutable.Buffer[Byte] = {
      val index = s.ordinal(t) // (2)
      summon[Encoder[Int]](index)
    }

  inline def encoderProduct[T](
      p: Mirror.ProductOf[T],
      encoders: => List[Encoder[_]]
  ): Encoder[T] = new Encoder[T]:
    def apply(t: T): mutable.Buffer[Byte] = {
      t.asInstanceOf[Product]
        .productIterator
        .zip(encoders.iterator)
        .map((p, e) => e.asInstanceOf[Encoder[Any]](p))
        .foldLeft(mutable.Buffer.empty)(_ ++ _)
    }

  inline def summonAll[T <: Tuple]: List[Encoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Encoder[t]] :: summonAll[ts]

  inline given derived[T](using m: Mirror.Of[T]): Encoder[T] =
    lazy val encoders = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => encoderSimplySum(s)
      case p: Mirror.ProductOf[T] => encoderProduct(p, encoders)

  inline given encodelist[T](using encoder: Encoder[T]): Encoder[List[T]] =
    new Encoder[List[T]]:
      def apply(t: List[T]): mutable.Buffer[Byte] =
        t.foldLeft(mutable.Buffer.empty)(_ ++ encoder(_))

end Encoder

trait Encoder[A] { self =>
  def apply(a: A): mutable.Buffer[Byte]

  def contramap[B](f: B => A): Encoder[B] = (a: B) => self(f(a))
}
