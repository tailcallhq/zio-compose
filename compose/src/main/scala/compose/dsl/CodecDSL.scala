package compose.dsl

import compose.ExecutionPlan.Codec
import compose.model.Decoder
import compose.model.Decoder.HasDecoder
import compose.{Lambda, ~>}
import zio.schema.{DynamicValue, Schema}

object CodecDSL {
  trait Op[-A, +B] {
    self: A ~> B =>

    def decode[C](implicit s: Schema[C], decoder: HasDecoder[B]): A ~> Either[String, C] =
      self >>> Lambda.unsafe.attempt[B, Either[String, C]] {
        Codec.Decode(s.ast, Schema[Decoder].toDynamic(decoder.decoder))
      }

    def encode: A ~> DynamicValue = self >>> Lambda.unsafe.attempt[B, DynamicValue] { Codec.Encode }

  }
}
