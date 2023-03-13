package org.quasigroup.ibclient.client.encoder

import fs2.Chunk
import scodec.bits.Literals.Utf8

import scala.collection.mutable.Buffer
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

  // inline def derive[T](m: Mirror.Of[T]): Encoder[T] =
  //   def deriveSum(s.)
  //   lazy val instances = summonAll[m.MirroredElemTypes]
  //   inline m match
  //     case s: Mirror.SumOf[T] => deriveSum(s, instances)
  //     case p: Mirror.Product[T] => deriveProduct(p, instances)
    
}

trait Encoder[A] { self =>
  def apply(a: A): Buffer[Byte]

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    final def apply(a: B):  Buffer[Byte] = self(f(a))
  }
}
