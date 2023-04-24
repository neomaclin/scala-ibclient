package org.quasigroup.ibclient.decoder

import org.quasigroup.ibclient.response.ResponseMsg
import org.quasigroup.ibclient.types.Decimal
import cats.data.{IndexedStateT, StateT}

import scala.reflect.ClassTag
import scala.compiletime.{summonFrom, erasedValue, summonInline}
import scala.deriving.Mirror
import scala.util.{Either, Try}

object Decoder {

  type ThrowableOr[A] = Either[Throwable, A]
  type DecoderState[T] = StateT[ThrowableOr, Array[String], T]

  inline def decode[A: Decoder](entry: Array[String]): Either[Throwable, A] =
    summon[Decoder[A]](entry)

  inline given Decoder[String] = (entry: Array[String]) => Right(entry.headOption.getOrElse(""))

  inline given Decoder[Long] =
    summon[Decoder[String]].flatMap(value => if value.isEmpty then Right(0) else Try(value.toLong).toEither)

  inline given Decoder[Int] =
    summon[Decoder[String]].flatMap(value => if value.isEmpty then Right(0) else Try(value.toInt).toEither)

  inline given Decoder[Boolean] = summon[Decoder[Int]].map(_ != 0)

  inline given Decoder[Double] = summon[Decoder[String]].flatMap(value =>
    if value.isEmpty then Right(0)
    else Try(value.toDouble).toEither
  )

  inline given Decoder[Decimal] = summon[Decoder[String]].flatMap(value =>
    if value.isEmpty || value == String.valueOf(
        Long.MaxValue
      ) || value == String.valueOf(Int.MaxValue) || value == String.valueOf(
        Double.MaxValue
      )
    then Right(Decimal.INVALID)
    else Decimal.parse(value)
  )

  import scala.compiletime.{erasedValue, summonInline}
  import scala.deriving.*

  inline def summonAllEnum[T <: Tuple](idx: Int = 0): Map[Int, _] =
    inline erasedValue[T] match
      case _: EmptyTuple => Map.empty[Int, T]
      case _: (t *: ts)  => summonAllEnum[ts](idx + 1) + (idx -> summonInline[t])

  inline def summonAllDecoder[T <: Tuple]: List[Decoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Decoder[t]] :: summonAllDecoder[ts]

  inline def readNothing[T: ClassTag](default: T): DecoderState[T] =
    StateT(array => Right(array -> default))

  inline def read[T: Decoder]: DecoderState[T] =
    StateT(array => summon[Decoder[T]](array).map(array.tail -> _))

  inline given decodeProduct[T](using mirror: Mirror.ProductOf[T]): Decoder[T] =
    new Decoder[T] {
      final def apply(entry: Array[String]): Either[Throwable, T] =
        summonAllDecoder[mirror.MirroredElemTypes].iterator
          .foldLeft(readNothing[Tuple](EmptyTuple)) { (state, e) =>
            for
              product <- state
              item <- StateT[ThrowableOr, Array[String], Any](array =>
                e.asInstanceOf[Decoder[Any]].apply(array).map(array.tail -> _)
              )
            yield product :* item
          }
          .runA(entry)
          .map(mirror.fromProduct)
    }

}

trait Decoder[A] { self =>

  def apply(entry: Array[String]): Either[Throwable, A]

  final def map[B](f: A => B): Decoder[B] = (entry: Array[String]) => self.apply(entry).map(f)
  final def flatMap[B](f: A => Either[Throwable, B]): Decoder[B] =
    (entry: Array[String]) => self.apply(entry).flatMap(f)
}
