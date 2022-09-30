package compose.model

import compose.macros.DeriveAccessors
import zio.Chunk
import zio.schema.{DeriveSchema, Schema}

object http {
  case class Request(
    method: Method = Method.Get,
    url: String,
    headers: Map[String, String] = Map.empty,
    body: Option[String] = None,
  )

  object Request {
    implicit val schema: Schema[Request] = DeriveSchema.gen[Request]
    val lens                             = DeriveAccessors.gen[Request]
  }

  sealed trait Method {
    def name: String = this match {
      case Method.Get    => "GET"
      case Method.Post   => "POST"
      case Method.Put    => "PUT"
      case Method.Delete => "DELETE"
    }
  }
  case object Method  {
    case object Get    extends Method
    case object Post   extends Method
    case object Put    extends Method
    case object Delete extends Method
  }

  case class Response(status: Int, headers: Map[String, String], body: Chunk[Byte])
  object Response {
    implicit val schema: Schema[Response] = DeriveSchema.gen[Response]
    val lens                              = DeriveAccessors.gen[Response]
  }
}
