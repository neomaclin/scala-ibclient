package org.quasigroup.ibclient.decoder

import org.quasigroup.ibclient.response.ResponseMsg
import org.quasigroup.ibclient.types.Decimal

import cats.data.{IndexedStateT, StateT}
import scala.reflect.ClassTag
//import cats.mtl.syntax.state

import scala.compiletime.summonFrom
import scala.deriving.Mirror
import scala.util.Try

object Decoder {

  type ThrowableOr[A] = Either[Throwable, A]
  type DecoderState[T] = StateT[ThrowableOr, Array[String], T]


  inline def decode[A: Decoder](entry: Array[String]): Either[Throwable, A] =
    summon[Decoder[A]](entry)

  inline given Decoder[String] = (entry: Array[String]) =>
    entry.headOption.toRight(new Exception("nothing to decode"))

  inline given Decoder[Long] = summon[Decoder[String]].flatMap(value =>
    if value.isEmpty then Right(Long.MaxValue) else Try(value.toLong).toEither
  )

  inline given Decoder[Int] = summon[Decoder[String]].flatMap(value =>
    if value.isEmpty then Right(Int.MaxValue) else Try(value.toInt).toEither
  )

  inline given Decoder[Boolean] = summon[Decoder[Int]].map(_ != 0)

  inline given Decoder[Double] = summon[Decoder[String]].flatMap(value =>
    if value.isEmpty then Right(Double.MaxValue)
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
  inline def summonAll[T <: Tuple]: List[Decoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Decoder[t]] :: summonAll[ts]

  // inline given decodeProduct[T](using mirror: Mirror.ProductOf[T]): Decoder[T] =
  //   new Decoder[T] {
  //     final def apply(entry: Array[String]): Either[Throwable, T] =
  //       entry.iterator
  //         .zip(summonAll[mirror.MirroredElemTypes].iterator)
  //         .map((s: String, e) => e.asInstanceOf[Decoder[Any]](Array(s)))
  //         .foldRight(Right(EmptyTuple): Either[Throwable, Tuple]) {
  //           case (Left(itemError), Left(accError)) => Left(itemError)
  //           case (Left(itemError), Right(product)) => Left(itemError)
  //           case (Right(item), Right(product))     => Right(item *: product)
  //           case (Right(item), Left(accError))     => Left(accError)
  //         }
  //         .map(mirror.fromProduct)
  //   }

   
  inline def readNothing[T: ClassTag](default: T): DecoderState[T] =
    StateT(array => Right(array -> default))

  inline def read[T: Decoder]: DecoderState[T] =
    StateT(array => summon[Decoder[T]](array).map(array.tail -> _))


  inline given decodeProduct[T](using mirror: Mirror.ProductOf[T]): Decoder[T] =
    new Decoder[T] {
      final def apply(entry: Array[String]): Either[Throwable, T] =
        summonAll[mirror.MirroredElemTypes].iterator.foldRight(readNothing[Tuple](EmptyTuple)){
          (e, state) =>
            for
              product <- state
              item <- StateT[ThrowableOr, Array[String],Any](array =>  e.asInstanceOf[Decoder[Any]](array).map(array.tail -> _))
            yield item *: product
        }.runA(entry).map(mirror.fromProduct)
    }

}

trait Decoder[A] { self =>

  def apply(entry: Array[String]): Either[Throwable, A]

  final def map[B](f: A => B): Decoder[B] = (entry: Array[String]) =>
    self.apply(entry).map(f)
  final def flatMap[B](f: A => Either[Throwable, B]): Decoder[B] =
    (entry: Array[String]) => self.apply(entry).flatMap(f)
}
