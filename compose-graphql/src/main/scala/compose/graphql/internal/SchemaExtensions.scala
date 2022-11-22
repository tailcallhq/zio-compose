package compose.graphql.internal

import zio.schema.Schema

object SchemaExtensions {
  implicit final class Extensions[A](self: Schema[A]) {
    def eval: Schema[A] = self match {
      case Schema.Lazy(schema) => schema()
      case schema              => schema
    }

    def isOption: Boolean = eval match {
      case _: Schema.Optional[_] => true
      case _                     => false
    }

    def isSeq: Boolean = eval match {
      case _: Schema.Sequence[_, _, _] => true
      case _                           => false
    }

    def isRecord: Boolean = eval match {
      case _: Schema.Record[_] => true
      case _                   => false
    }
  }
}
