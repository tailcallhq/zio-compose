package compose.model

import compose.model.http.Response
import zio.schema.{DeriveSchema, DynamicValue, Schema}

sealed trait Decoder

object Decoder {
  sealed trait HasDecoder[-A] extends Decoder {
    self =>
    def decoder: Decoder = self
  }

  object HasDecoder {
    implicit case object ResponseDecoder     extends HasDecoder[Response]
    implicit case object DynamicValueDecoder extends HasDecoder[DynamicValue]
  }

  implicit val schema: Schema[Decoder] = DeriveSchema.gen[Decoder]
}
