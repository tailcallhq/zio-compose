package compose.graphql

import compose.{Lambda, ~>}
import zio.schema.Schema

/**
 * A `Connection` represents a connection between two nodes
 * in a graph.
 */
sealed trait Connection

object Connection {
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

  def arg[A, B](name: String, ab: A ~> B)(implicit from: Schema[A], to: Schema[B]): Connection =
    Connection[A, Unit, B](name, Lambda.identity[(A, Unit)]._1 >>> ab)

  def from[A, B](name: String, ab: A ~> B)(implicit from: Schema[A], to: Schema[B]): Connection =
    Connection[Unit, A, B](name, Lambda.identity[(Unit, A)]._2 >>> ab)
}
