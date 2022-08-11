package compose

import zio.schema.{DeriveSchema, DynamicValue, Schema}

sealed trait IsNumeric[A] { self =>
  def one: A
  def toDynamic: DynamicValue = IsNumeric.schema.toDynamic(self)
}

object IsNumeric {
  implicit case object NumericInt extends IsNumeric[Int] {
    override def one: Int = 1
  }

  implicit def schema: Schema[IsNumeric[_]] = DeriveSchema.gen[IsNumeric[_]]
}
