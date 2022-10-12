package compose.graphql

import compose.{Lambda, ~>}
import zio.schema.Schema

case class Path[Arg, From, To](resolve: (Arg, From) ~> To) {
  def connect(
    name: String,
  )(implicit arg: Schema[Arg], from: Schema[From], to: Schema[To]): Connection =
    Connection(name, resolve)
}

object Path {

  def arg[A, B](ab: => A ~> B): Path[A, Unit, B] = Path(Lambda.identity[(A, Unit)]._1 >>> ab)

  def from[A, B](ab: => A ~> B): Path[Unit, A, B] = Path(Lambda.identity[(Unit, A)]._2 >>> ab)

  def make[A, B, C](ab: (A, B) ~> C): Path[A, B, C] = Path(ab)
}
