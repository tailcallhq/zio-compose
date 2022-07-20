package com.tusharmath.compose

sealed trait IsNumeric[A]
object IsNumeric {
  implicit case object NumericInt    extends IsNumeric[Int]
  implicit case object NumericLong   extends IsNumeric[Long]
  implicit case object NumericDouble extends IsNumeric[Double]
  implicit case object NumericFloat  extends IsNumeric[Float]
  implicit case object NumericBigInt extends IsNumeric[BigInt]
}
