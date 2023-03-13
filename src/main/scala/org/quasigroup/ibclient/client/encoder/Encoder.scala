package org.quasigroup.ibclient.client.encoder

import fs2.Chunk
import scodec.bits.Literals.Utf8

import scala.collection.mutable.ArrayBuffer

object Encoder {


  opaque type Length = Int
  object Length:
    def apply(value: Int): Length = value

  given LengthEncoder: Encoder[Length] = (length: Length) => Chunk.array(Array((0xff & (length >> 24)).toByte,
    (0xff & (length >> 16)).toByte,
    (0xff & (length >> 8)).toByte,
    (0xff & length).toByte))

  given StringEncoder : Encoder[String] = (value: String) => Chunk.array(value.getBytes(Utf8))

  given BytesEncoder : Encoder[Array[Byte]] = (value: Array[Byte]) => Chunk.array(value)

  given BooleanEncoder : Encoder[Boolean] = IntEncoder.contramap(if _ then 1 else 0)

  given DoubleEncoder : Encoder[Double] = StringEncoder.contramap(_.toString)

  def encodeList[T:Encoder]: Encoder[List[T]] = (values: List[T]) =>
    Chunk.array(values.foldLeft(ArrayBuffer.empty[Byte])(_.addAll(Encoder[T](_))).toArray)

}

trait Encoder[A] {
  def apply(a: A): Chunk[Byte]

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    final def apply(a: B): Chunk[Byte] = self(f(a))
  }
}
