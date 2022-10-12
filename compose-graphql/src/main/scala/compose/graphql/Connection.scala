package compose.graphql

import compose.~>
import zio.schema.{DeriveSchema, Schema}

sealed trait Connection {
  self =>
}

object Connection {
  case class Query()
  object Query {
    implicit val schema: Schema[Query] = DeriveSchema.gen[Query]
  }

  final case class Cons[Arg, From, To](
    name: String,
    arg: Schema[Arg],
    from: Schema[From],
    to: Schema[To],
    resolve: (Arg, From) ~> To,
  ) extends Connection

  def apply[Arg, From, To](name: String, resolve: (Arg, From) ~> To)(implicit
    arg: Schema[Arg],
    from: Schema[From],
    to: Schema[To],
  ): Connection = Cons(name, arg, from, to, resolve)
}
