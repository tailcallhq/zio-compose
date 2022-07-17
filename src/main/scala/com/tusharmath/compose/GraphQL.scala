package com.tusharmath.compose

import zio.schema.Schema

sealed trait GraphQL[-A, +B] {}

object GraphQL {
  case class Constant[B](b: B, schema: Schema[B])                 extends GraphQL[Any, B]
  case class Identity[A](schema: Schema[A])                       extends GraphQL[A, A]
  case class Compose[A, B, C](g: GraphQL[B, C], f: GraphQL[A, B]) extends GraphQL[A, C]
  case class Zip2[A, B, C](g: GraphQL[A, B], f: GraphQL[A, C])    extends GraphQL[A, (B, C)]
  case class Load[A](endpoint: Endpoint, schema: Schema[A])       extends GraphQL[Any, A]

  def constant[A](a: A)(implicit schema: Schema[A]): GraphQL[Any, A] = Constant(a, schema)
}
