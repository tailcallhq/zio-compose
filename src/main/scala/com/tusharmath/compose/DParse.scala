package com.tusharmath.compose

import zio.schema.{DynamicValue, Schema}
import zio.schema.ast.SchemaAst

object DParse {
  def toInt(dv: DynamicValue): Either[String, Int] =
    dv.toTypedValue(Schema[Int])

  def toBoolean(dv: DynamicValue): Either[String, Boolean] =
    dv.toTypedValue(Schema[Boolean])

  def toAny(dv: DynamicValue, ast: SchemaAst): Either[String, Any] =
    dv.toTypedValue(ast.toSchema)
}
