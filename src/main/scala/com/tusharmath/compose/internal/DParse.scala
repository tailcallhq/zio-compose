package com.tusharmath.compose.internal

import zio.schema.{DynamicValue, Schema}

object DParse {

  def toIntTuple(dv: DynamicValue): Either[String, (Int, Int)] =
    toDVTuple(dv).flatMap { case (d1, d2) => toInt(d1).zip(toInt(d2)) }

  def toInt(dv: DynamicValue): Either[String, Int] =
    dv.toTypedValue(Schema[Int])

  private def toDVTuple(
    dv: DynamicValue,
  ): Either[String, (DynamicValue, DynamicValue)] = {
    val p1 = DExtract.paramIdN(1, dv)
    val p2 = DExtract.paramIdN(2, dv)
    p1.zip(p2)
  }

  def toBooleanTuple(dv: DynamicValue): Either[String, (Boolean, Boolean)] =
    toDVTuple(dv).flatMap { case (d1, d2) => toBoolean(d1).zip(toBoolean(d2)) }

  def toBoolean(dv: DynamicValue): Either[String, Boolean] =
    dv.toTypedValue(Schema[Boolean])
}
