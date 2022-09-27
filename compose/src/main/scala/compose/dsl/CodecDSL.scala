package compose.dsl

import compose.ExecutionPlan.Codec
import compose.Lambda.attempt
import compose.~>
import zio.schema
import zio.schema.{DynamicValue, Schema}

trait CodecDSL[-A, +B] { self: A ~> B =>

  def decode[C](implicit s: Schema[C], ev: B <:< schema.DynamicValue): A ~> Either[String, C] =
    self >>> attempt[B, Either[String, C]] { Codec.Decode(s.ast) }

  def encode: A ~> DynamicValue = self >>> attempt[B, DynamicValue] { Codec.Encode }

}
