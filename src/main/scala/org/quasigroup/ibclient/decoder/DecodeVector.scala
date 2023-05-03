package org.quasigroup.ibclient.decoder

import scodec.bits.ByteVector

object DecodeVector {

  inline given DecodeVector[String] with
    val codec = scodec.codecs.utf8 <~ scodec.codecs.constant(0.toChar)

  inline given DecodeVector[Int] = summon[DecodeVector[String]].map(_.toInt)

}

trait DecodeVector[A] { self =>

  def codec: scodec.Decoder[A]

  final def map[B](f: A => B): DecodeVector[B] = new DecodeVector[B] {
    override final val codec: scodec.Decoder[B] = self.codec.map(f)
  }

  final def flatMap[B](f: A => DecodeVector[B]): DecodeVector[B] = new DecodeVector[B] {
    override final val codec: scodec.Decoder[B] = self.codec.flatMap(f(_).codec)
  }
}
