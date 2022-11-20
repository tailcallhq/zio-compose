package compose.graphql

import compose.{ExecutionPlan, ~>}
import zio.Chunk
import zio.schema.codec.JsonCodec.JsonEncoder
import zio.schema.{DeriveSchema, Schema}

/**
 * A `GraphQL` represents a connection between two nodes in
 * a graph.
 */

sealed trait Edge {
  self =>
  final def ++(other: Edge): Edge      = Edge.Concat(self, other)
  final def combine(other: Edge): Edge = self ++ other
  final def cons: Chunk[Edge.Cons]     = self match {
    case self: Edge.Cons          => Chunk.single(self)
    case Edge.Concat(left, right) => left.cons ++ right.cons
    case Edge.Empty               => Chunk.empty
  }
  final def binary: Chunk[Byte]        = JsonEncoder.encode(Edge.schema, self)
  final def toJson: String             = new String(binary.toArray)
}

object Edge {
  final case class Cons(
    name: String,
    argType: Schema[Any],
    fromType: Schema[Any],
    toType: Schema[Any],
    executable: ExecutionPlan,
  ) extends Edge

  case object Empty                                extends Edge
  final case class Concat(left: Edge, right: Edge) extends Edge

  def apply[Arg, From]: PartiallyAppliedConnection[Arg, From] =
    new PartiallyAppliedConnection[Arg, From](())

  final class PartiallyAppliedConnection[Arg, From](val unit: Unit) extends AnyVal {
    def apply[To](name: String, resolve: (Arg, From) ~> To)(implicit
      arg: Schema[Arg],
      from: Schema[From],
      to: Schema[To],
    ): Edge = Cons(
      name,
      arg.asInstanceOf[Schema[Any]],
      from.asInstanceOf[Schema[Any]],
      to.asInstanceOf[Schema[Any]],
      resolve.compile,
    )
  }

  def empty: Edge = Empty

  implicit val schema = DeriveSchema.gen[Edge]
}
