package org.quasigroup.ibclient.client.encoder

import fs2.Chunk
import scodec.bits.Literals.Utf8

import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.compiletime.summonFrom
import scala.deriving.Mirror

object Encoder {


  opaque type Length = Int
  object Length:
    def apply(value: Int): Length = value

  given LengthEncoder: Encoder[Length] = (length: Length) => Array((0xff & (length >> 24)).toByte,
    (0xff & (length >> 16)).toByte,
    (0xff & (length >> 8)).toByte,
    (0xff & length).toByte).toBuffer

  given StringEncoder : Encoder[String] = _.getBytes.toBuffer

  given IntEncoder : Encoder[Int] = StringEncoder.contramap(_.toString)

  given BytesEncoder : Encoder[Array[Byte]] = _.toBuffer

  given BooleanEncoder : Encoder[Boolean] = IntEncoder.contramap(if _ then 1 else 0)

  given DoubleEncoder : Encoder[Double] = StringEncoder.contramap(_.toString)

  given encodeList[T](using e:Encoder[T]): Encoder[List[T]] = _.foldLeft(Buffer.empty)(_ ++ e(_))

  import scala.compiletime.{erasedValue, summonInline}

  def encoderSum[T](s: Mirror.SumOf[T], encoders: => List[Encoder[_]]): Encoder[T] = new Encoder[T]:
      def apply(t: T): Buffer[Byte] = {
        val index = s.ordinal(t) // (2)
        encoders(index).asInstanceOf[Encoder[Any]](t) // (3)
      }
  def encoderProduct[T](p: Mirror.ProductOf[T], encoders: => List[Encoder[_]]): Encoder[T] = new Encoder[T]:
      def apply(t: T): Buffer[Byte] = {
        t.asInstanceOf[Product].productIterator.zip(encoders.iterator)
          .map((p, e) => e.asInstanceOf[Encoder[Any]](p)).foldLeft(Buffer.empty)(_ ++ _)
      }
  inline def summonAll[T <: Tuple]: List[Encoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Encoder[t]] :: summonAll[ts]

  inline given derived[T](using m: Mirror.Of[T]): Encoder[T] =
    lazy val encoders = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T] => encoderSum(s, encoders)
      case p: Mirror.ProductOf[T] => encoderProduct(p, encoders)


}

trait Encoder[A] { self =>
  def apply(a: A): Buffer[Byte]

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    final def apply(a: B):  Buffer[Byte] = self(f(a))
  }
}
