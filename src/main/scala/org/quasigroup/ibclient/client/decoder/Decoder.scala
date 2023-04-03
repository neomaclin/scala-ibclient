package org.quasigroup.ibclient.client.decoder

object Decoder {
  def decode[A:Decoder](entry: Array[String]): Either[Throwable,A] = summon[Decoder[A]](entry)
}

trait Decoder[A]{
  def apply(entry: Array[String]): Either[Throwable, A]
}
