package org.quasigroup.ibclient.client.types

import java.math.MathContext
import java.text.DecimalFormat
import scala.util.Try

opaque type Decimal = BigDecimal

extension (decimal: Decimal) {
  def value: BigDecimal = decimal
  def isZero: Boolean =
    decimal == Decimal.ZERO || decimal.value.bigDecimal.signum() == 0
  def isValid: Boolean = decimal != Decimal.INVALID || decimal != Decimal.NaN
}

object Decimal:

  // val MINUS_ONE = apply(BigDecimal.valueOf(-1))
  val MATH_CONTEXT: MathContext = MathContext.DECIMAL64
  val ZERO: Decimal = BigDecimal.valueOf(0)
  val ONE: Decimal = BigDecimal.valueOf(1)
  val NaN: Decimal = BigDecimal.valueOf(Long.MinValue)

  private val NAN_STRING: String =
    java.lang.Double.toString(java.lang.Double.NaN)

  val INVALID: Decimal = apply(BigDecimal.valueOf(Long.MinValue))

  def parse(str: String): Either[Throwable, Decimal] =
    if str.isEmpty then Left(new Exception("nothing to parse"))
    else if NAN_STRING == str then Right(NaN)
    else {
      val text = str.trim().replaceAll(",", "")
      Try(
        new java.math.BigDecimal(
          text.toCharArray,
          0,
          text.length(),
          MATH_CONTEXT
        )
      ).map(new scala.math.BigDecimal(_)).toEither
    }

  def apply(value: BigDecimal): Decimal = value

//  def apply(value: Long): Decimal = if (v == Double.MAX_VALUE) {
//             result = INVALID;
//         } else if (v == 0) {
//             result = ZERO;
//         } else if (Double.isNaN(v) || Double.isInfinite(v)) {
//             result = NaN;
//         } else {
//             DecimalFormat df = new DecimalFormat("#");
//             df.setMaximumFractionDigits(16);
//             result = Decimal.parse(df.format(v));
//         }
//         return result;

  def parse(v: Double): Either[Throwable, Decimal] =
    if v == Double.MaxValue then Right(INVALID)
    else if v.isZero then Right(ZERO)
    else if v.isNaN || v.isInfinite then Right(NaN)
    else {
      val df = new DecimalFormat("#")
      df.setMaximumFractionDigits(16)
      parse(df.format(v))
    }

  // def apply(value: BigDecimal): Decimal = value
