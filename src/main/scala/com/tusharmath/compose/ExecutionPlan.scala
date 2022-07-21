package com.tusharmath.compose
import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.ast.SchemaAst
import zio.schema.codec.JsonCodec
import zio.{Chunk, Task}

sealed trait ExecutionPlan { self =>
  def json: String                                                       = new String(binary.toArray)
  def binary: Chunk[Byte]                                                = JsonCodec.encode(ExecutionPlan.schema)(self)
  def unsafeExecute[A](A: A)(implicit ev: Schema[A]): Task[DynamicValue] = Executor.execute(self, ev.toDynamic(A))
}

object ExecutionPlan {

  def fromGraphQL[A, B](graphQL: ZLambda[A, B]): ExecutionPlan =
    graphQL match {
      case ZLambda.Pipe(f, g)            => Sequence(fromGraphQL(f), fromGraphQL(g))
      case ZLambda.Zip2(f1, f2, s1, s2)  => Combine(fromGraphQL(f1), fromGraphQL(f2), s1.ast, s2.ast)
      case ZLambda.FromMap(i, source, o) => Dictionary(source.map { case (k, v) => (i.toDynamic(k), o.toDynamic(v)) })
      case ZLambda.Select(input, path, output) => Select(path)
      case ZLambda.Constant(b, schema)         => Constant(schema.toDynamic(b))
      case ZLambda.Identity()                  => Identity
      case ZLambda.AddInt                      => AddInt
    }

  case class Constant(value: DynamicValue)                                               extends ExecutionPlan
  case class Combine(e1: ExecutionPlan, e2: ExecutionPlan, s1: SchemaAst, s2: SchemaAst) extends ExecutionPlan
  case class Sequence(first: ExecutionPlan, second: ExecutionPlan)                       extends ExecutionPlan
  case class Dictionary(value: Map[DynamicValue, DynamicValue])                          extends ExecutionPlan
  case class Select(path: List[String])                                                  extends ExecutionPlan
  case object AddInt                                                                     extends ExecutionPlan
  case object Identity                                                                   extends ExecutionPlan

  implicit val schema = DeriveSchema.gen[ExecutionPlan]
}
