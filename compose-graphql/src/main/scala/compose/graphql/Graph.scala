package compose.graphql

import compose.{ExecutionPlan, ~>}

import zio.schema.codec.JsonCodec.JsonEncoder
import zio.schema.{DeriveSchema, Schema}
import zio.Chunk

/**
 * A `GraphQL` represents a connection between two nodes in
 * a graph.
 */

sealed trait Graph {
  self =>
  final def ++(other: Graph): Graph      = Graph.Concat(self, other)
  final def combine(other: Graph): Graph = self ++ other
  final def cons: Chunk[Graph.Cons]      = self match {
    case self: Graph.Cons          => Chunk.single(self)
    case Graph.Concat(left, right) => left.cons ++ right.cons
    case Graph.Empty               => Chunk.empty
  }
  final def binary: Chunk[Byte]          = JsonEncoder.encode(Graph.schema, self)
  final def toJson: String               = new String(binary.toArray)
  final def add[Arg, From]: Graph.PartiallyAppliedConnection[Arg, From] =
    new Graph.PartiallyAppliedConnection[Arg, From](self)
}

object Graph {
  final case class Cons(
    name: String,
    argType: Schema[Any],
    fromType: Schema[Any],
    toType: Schema[Any],
    executable: ExecutionPlan,
  ) extends Graph

  case object Empty                                  extends Graph
  final case class Concat(left: Graph, right: Graph) extends Graph

  def apply[Arg, From]: PartiallyAppliedConnection[Arg, From] =
    new PartiallyAppliedConnection[Arg, From](empty)

  final class PartiallyAppliedConnection[Arg, From](val graph: Graph) extends AnyVal {
    def apply[To](name: String, resolve: (Arg, From) ~> To)(implicit
      arg: Schema[Arg],
      from: Schema[From],
      to: Schema[To],
    ): Graph = graph ++ Cons(
      name,
      arg.asInstanceOf[Schema[Any]],
      from.asInstanceOf[Schema[Any]],
      to.asInstanceOf[Schema[Any]],
      resolve.compile,
    )
  }

  def empty: Graph = Empty

  implicit val schema = DeriveSchema.gen[Graph]
}
