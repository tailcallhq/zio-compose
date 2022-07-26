package compose

import zio.schema.{DeriveSchema, Schema}

object Numeric {

  sealed trait IsNumeric[A]

  object IsNumeric {
    case object NumericInt extends IsNumeric[Int]

    implicit def schema: Schema[IsNumeric[_]] = DeriveSchema.gen[IsNumeric[_]]
  }

  sealed trait Operation
  object Operation {
    case object Add      extends Operation
    case object Multiply extends Operation
    case object Subtract extends Operation
    case object Divide   extends Operation
  }
}
