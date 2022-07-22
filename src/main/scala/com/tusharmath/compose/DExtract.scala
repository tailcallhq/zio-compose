package com.tusharmath.compose

import zio.schema.DynamicValue

object DExtract {
  def paramIdN(n: Int, dv: DynamicValue): Either[String, DynamicValue] = {
    dv match {
      case DynamicValue.Record(values)            =>
        values.get("_" + n.toString) match {
          case Some(value) => Right(value)
          case None        => Left(s"Could not extract _${n} from: ${dv}")
        }
      case DynamicValue.Tuple(left, _) if n == 1  => Right(left)
      case DynamicValue.Tuple(_, right) if n == 2 => Right(right)
      case _ => Left(s"Could not extract _${n} from: ${dv}")
    }
  }
}
