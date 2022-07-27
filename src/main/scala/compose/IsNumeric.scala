package compose

import zio.schema.{DeriveSchema, Schema}

sealed trait IsNumeric[A] {
  def one: A
}

object IsNumeric {
  implicit case object NumericInt extends IsNumeric[Int] {
    override def one: Int = 1
  }

  implicit def schema: Schema[IsNumeric[_]] = DeriveSchema.gen[IsNumeric[_]]
}
