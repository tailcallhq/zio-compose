package compose.graphql

import compose.~>
import zio.Chunk
import zio.schema.Schema

/**
 * A `GraphQL` represents a connection between two nodes in
 * a graph.
 */
sealed trait Edge {
  self =>
  def ++(other: Edge): Edge           = Edge.Concat(self, other)
  def combine(other: Edge): Edge      = self ++ other
  def cons: Chunk[Edge.Cons[_, _, _]] = self match {
    case self: Edge.Cons[_, _, _] => Chunk.single(self)
    case Edge.Concat(left, right) => left.cons ++ right.cons
    case Edge.Empty               => Chunk.empty
  }
}

object Edge {
  final case class Cons[Arg, From, To](
    name: String,
    arg: Schema[Arg],
    from: Schema[From],
    to: Schema[To],
    resolve: (Arg, From) ~> To,
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
    ): Edge = Cons(name, arg, from, to, resolve)
  }

  def empty: Edge = Empty
}
