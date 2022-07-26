package compose

import zio.schema.{DeriveSchema, Schema}

sealed trait IsNumeric[A]

object IsNumeric {
  implicit case object NumericInt extends IsNumeric[Int]

  implicit def schema: Schema[IsNumeric[_]] = DeriveSchema.gen[IsNumeric[_]]
}
