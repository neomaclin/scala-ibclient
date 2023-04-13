package org.quasigroup.ibclient.client.decoder

import org.quasigroup.ibclient.client.response.ResponseMsg
import org.quasigroup.ibclient.client.types.Decimal

import scala.compiletime.summonFrom
import scala.deriving.Mirror
import scala.util.Try

object Decoder {

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

  inline given decodeProduct[T](using mirror: Mirror.ProductOf[T]): Decoder[T] =
    new Decoder[T] {
      final def apply(entry: Array[String]): Either[Throwable, T] =
        entry.iterator
          .zip(summonAll[mirror.MirroredElemTypes].iterator)
          .map((s: String, e) => e.asInstanceOf[Decoder[Any]](Array(s)))
          .foldRight(Right(EmptyTuple): Either[Throwable, Tuple]) {
            case (Left(itemError), Left(accError)) => Left(itemError)
            case (Left(itemError), Right(product)) => Left(itemError)
            case (Right(item), Right(product))     => Right(item *: product)
            case (Right(item), Left(accError))     => Left(accError)
          }
          .map(mirror.fromProduct)
    }

}

trait Decoder[A] { self =>

  def apply(entry: Array[String]): Either[Throwable, A]

  final def map[B](f: A => B): Decoder[B] = (entry: Array[String]) =>
    self.apply(entry).map(f)
  final def flatMap[B](f: A => Either[Throwable, B]): Decoder[B] =
    (entry: Array[String]) => self.apply(entry).flatMap(f)
}
