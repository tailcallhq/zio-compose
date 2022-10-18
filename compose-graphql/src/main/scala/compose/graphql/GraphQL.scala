package compose.graphql

import compose.{Lambda, ~>}
import zio.schema.Schema

/**
 * A `GraphQL` represents a connection between two nodes in
 * a graph.
 */
sealed trait GraphQL

object GraphQL {
  final case class Cons[Arg, From, To](
    name: String,
    arg: Schema[Arg],
    from: Schema[From],
    to: Schema[To],
    resolve: (Arg, From) ~> To,
  ) extends GraphQL

  def apply[Arg, From, To](name: String, resolve: (Arg, From) ~> To)(implicit
    arg: Schema[Arg],
    from: Schema[From],
    to: Schema[To],
  ): GraphQL = Cons(name, arg, from, to, resolve)

  def arg[Arg, To](name: String, ab: Arg ~> To)(implicit
    from: Schema[Arg],
    to: Schema[To],
  ): GraphQL = GraphQL[Arg, Unit, To](name, Lambda.identity[(Arg, Unit)]._1 >>> ab)

  def from[From, To](name: String, ab: From ~> To)(implicit
    from: Schema[From],
    to: Schema[To],
  ): GraphQL = GraphQL[Unit, From, To](name, Lambda.identity[(Unit, From)]._2 >>> ab)

  def root[To](name: String, ab: Any ~> To)(implicit to: Schema[To]): GraphQL =
    GraphQL[Unit, Unit, To](name, Lambda.identity[(Unit, Unit)]._2 >>> ab)
}
