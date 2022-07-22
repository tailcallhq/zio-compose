package com.tusharmath.compose

import zio.schema.DynamicValue

object DExtract {
  def paramIdN(n: Int, dv: DynamicValue): Either[String, DynamicValue] = {
    dv match {
      case DynamicValue.Record(values) =>
        values.get("_" + n.toString) match {
          case Some(value) => Right(value)
          case None        => Left(s"Could not extract _${n}")
        }
      case _                           => Left(s"Could not extract _${n}")
    }
  }
}
