package com.tusharmath.compose
import zio.schema.{DeriveSchema, DynamicValue}

sealed trait ExecutionPlan {}

object ExecutionPlan {

  implicit val schema = DeriveSchema.gen[ExecutionPlan]

  def fromGraphQL[A, B](graphQL: ZLambda[A, B]): ExecutionPlan =
    graphQL match {
      case ZLambda.Pipe(f, g)            => Sequence(fromGraphQL(f), fromGraphQL(g))
      case ZLambda.Zip2(g, f)            => Combine(fromGraphQL(g), fromGraphQL(f))
      case ZLambda.FromMap(i, source, o) => Dictionary(source.map { case (k, v) => (i.toDynamic(k), o.toDynamic(v)) })
      case ZLambda.Select(input, path, output) => Select(path)
      case ZLambda.Constant(b, schema)         => Constant(schema.toDynamic(b))
      case ZLambda.Identity()                  => Identity
      case ZLambda.Add(a, b, schema, standard) => Add(schema.toDynamic(a), schema.toDynamic(b), standard.tag)
    }

  case class Constant(value: DynamicValue)                         extends ExecutionPlan
  case class Combine(left: ExecutionPlan, right: ExecutionPlan)    extends ExecutionPlan
  case class Sequence(first: ExecutionPlan, second: ExecutionPlan) extends ExecutionPlan
  case class Dictionary(value: Map[DynamicValue, DynamicValue])    extends ExecutionPlan
  case class Select(path: List[String])                            extends ExecutionPlan
  case object Identity                                             extends ExecutionPlan
  case class Add(a: DynamicValue, b: DynamicValue, tag: String)    extends ExecutionPlan

}
