package compose

import zio.schema.{DeriveSchema, Schema}

sealed trait CanConcat[A] {}

object CanConcat {
  implicit case object ConcatString extends CanConcat[String]

  implicit val schema: Schema[CanConcat[_]] = DeriveSchema.gen[CanConcat[_]]
}
