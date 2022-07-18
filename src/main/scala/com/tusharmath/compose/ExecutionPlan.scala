package com.tusharmath.compose
import zio.schema.{DeriveSchema, DynamicValue}

sealed trait ExecutionPlan {}

object ExecutionPlan {

  implicit val schema = DeriveSchema.gen[ExecutionPlan]

  def fromGraphQL[A, B](graphQL: GraphQL[A, B]): ExecutionPlan =
    graphQL match {
      case GraphQL.Pipe(f, g)            => Sequence(fromGraphQL(f), fromGraphQL(g))
      case GraphQL.Zip2(g, f)            => Combine(fromGraphQL(g), fromGraphQL(f))
      case GraphQL.FromMap(i, source, o) => Dictionary(source.map { case (k, v) => (i.toDynamic(k), o.toDynamic(v)) })
      case GraphQL.Select(input, path, output) => Select(path)
      case GraphQL.Constant(b, schema)         => Constant(schema.toDynamic(b))
      case GraphQL.Identity()                  => Identity
    }

  case class Constant(value: DynamicValue)                         extends ExecutionPlan
  case class Combine(left: ExecutionPlan, right: ExecutionPlan)    extends ExecutionPlan
  case class Sequence(first: ExecutionPlan, second: ExecutionPlan) extends ExecutionPlan
  case class Dictionary(value: Map[DynamicValue, DynamicValue])    extends ExecutionPlan
  case class Select(path: List[String])                            extends ExecutionPlan
  case object Identity                                             extends ExecutionPlan
}
