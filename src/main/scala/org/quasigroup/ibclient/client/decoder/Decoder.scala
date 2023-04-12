package org.quasigroup.ibclient.client.decoder

import org.quasigroup.ibclient.client.response.{MsgReader, ResponseMsg}
import org.quasigroup.ibclient.client.types.Decimal

import scala.compiletime.summonFrom
import scala.deriving.Mirror
import scala.util.Try

object Decoder {
  def partiallyApply[T](
      entry: Array[String],
      matching: PartialFunction[Array[String], Either[Throwable, T]]
  ): Either[Throwable, T] =
    matching.applyOrElse(entry, _ => Left(new Exception("msg format error")))

  def decodeMsg(entry: Array[String]): Either[Throwable, ResponseMsg] =
    partiallyApply(
      entry,
      { case Array(msgId, rest: _*) =>
        MsgReader.read(msgId.toInt, rest.toArray)
      }
    )

  def decode[A: Decoder](entry: Array[String]): Either[Throwable, A] =
    summon[Decoder[A]](entry)

  given StringDecoder: Decoder[String] = (entry: Array[String]) =>
    entry.headOption.toRight(new Exception("nothing to decode"))

  given LongDecoder: Decoder[Long] = StringDecoder.flatMap(value =>
    if value.isEmpty then Right(Long.MaxValue) else Try(value.toLong).toEither
  )

  given IntDecoder: Decoder[Int] = StringDecoder.flatMap(value =>
    if value.isEmpty then Right(Int.MaxValue) else Try(value.toInt).toEither
  )

  given BooleanDecoder: Decoder[Boolean] = IntDecoder.map(_ != 0)

  given DoubleDecoder: Decoder[Double] = StringDecoder.flatMap(value =>
    if value.isEmpty then Right(Double.MaxValue)
    else Try(value.toDouble).toEither
  )

  given DecimalDecoder: Decoder[Decimal] = StringDecoder.flatMap(value =>
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

  inline given decodeSimpleSum[T](using
      mirror: Mirror.SumOf[T],
      gen: Int => T
  ): Decoder[T] = IntDecoder.map(gen)
}

trait Decoder[A] { self =>

  def apply(entry: Array[String]): Either[Throwable, A]

  final def map[B](f: A => B): Decoder[B] = (entry: Array[String]) =>
    self.apply(entry).map(f)
  final def flatMap[B](f: A => Either[Throwable, B]): Decoder[B] =
    (entry: Array[String]) => self.apply(entry).flatMap(f)
}
