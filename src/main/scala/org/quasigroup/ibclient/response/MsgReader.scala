package org.quasigroup.ibclient.response

import ResponseMsg.*
import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given
import org.quasigroup.ibclient.decoder.Decoder
import org.quasigroup.ibclient.decoder.Decoder.given
import org.quasigroup.ibclient.response.MsgDecoders.given
import cats.data.{IndexedStateT, StateT}
import scala.reflect.ClassTag

type ThrowableOr[A] = Either[Throwable, A]
type DecoderState[T] = StateT[ThrowableOr, Array[String], T]

object MsgReader:
   
  inline def readNothing[T: ClassTag](default: T): DecoderState[T] =
    StateT(array => Right(array -> default))

  inline def read[T: Decoder]: DecoderState[T] =
    StateT(array => summon[Decoder[T]](array).map(array.tail -> _))


  

