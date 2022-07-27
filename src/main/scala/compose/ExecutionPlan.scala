package compose

import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.{Chunk, ZIO}
import zio.schema.ast.SchemaAst

sealed trait ExecutionPlan { self =>
  def binary: Chunk[Byte] = JsonCodec.encode(ExecutionPlan.schema)(self)
  def json: String        = new String(binary.toArray)
}

object ExecutionPlan {

  implicit def schema: Schema[ExecutionPlan] = DeriveSchema.gen[ExecutionPlan]

  def fromJson(json: String): ZIO[Any, Exception, ExecutionPlan] =
    JsonCodec.decode(ExecutionPlan.schema)(
      Chunk.fromArray(json.getBytes),
    ) match {
      case Left(value)  => ZIO.fail(new Exception(value))
      case Right(value) => ZIO.succeed(value)
    }

  def fromLambda[A, B](lmb: Lambda[A, B]): ExecutionPlan = lmb match {
    case Lambda.Equals(left, right) =>
      Equals(left.compile, right.compile)

    case Lambda.FromMap(input: Schema[A] @unchecked, source, output: Schema[B] @unchecked) =>
      FromMap(source.map { case (a, b) => (input.toDynamic(a), output.toDynamic(b)) })

    case Lambda.Constant(b: B @unchecked, schema: Schema[B] @unchecked) =>
      Constant(schema.toDynamic(b))

    case Lambda.Identity() =>
      Identity

    case Lambda.Pipe(f, g) =>
      Pipe(f.compile, g.compile)

    case Lambda.GetPath(_, path, _) =>
      GetPath(path)

    case Lambda.SetPath(_, path, _) =>
      SetPath(path)

    case Lambda.IfElse(f, isTrue, isFalse) =>
      IfElse(f.compile, isTrue.compile, isFalse.compile)

    case Lambda.Combine(left, right, o1, o2) =>
      Combine(left.compile, right.compile, o1.ast, o2.ast)

    case Lambda.NumericOperation(operation, left, right, num) =>
      NumericOperation(
        operation,
        left.compile,
        right.compile,
        IsNumeric.schema.toDynamic(num),
      )

    case Lambda.LogicalAnd(left, right) =>
      LogicalAnd(left.compile, right.compile)

    case Lambda.LogicalOr(left, right) =>
      LogicalOr(left.compile, right.compile)

    case Lambda.LogicalNot(logic) =>
      LogicalNot(logic.compile)

    case Lambda.Transformation(transformations, output) =>
      Transformation(
        transformations.map { case Lambda.Transform.Constructor(f, i, g) => (f.compile, i.ast, g.compile) },
        output.ast,
      )

    case Lambda.Default(schema) =>
      schema.defaultValue match {
        case Left(cause)  => throw new Exception(cause)
        case Right(value) => Default(schema.toDynamic(value))
      }
  }

  final case class Default(value: DynamicValue) extends ExecutionPlan

  final case class Transformation(
    transformations: List[(ExecutionPlan, SchemaAst, ExecutionPlan)],
    wholeTargetAst: SchemaAst,
  ) extends ExecutionPlan

  final case class LogicalAnd(left: ExecutionPlan, right: ExecutionPlan) extends ExecutionPlan

  final case class LogicalOr(left: ExecutionPlan, right: ExecutionPlan) extends ExecutionPlan

  final case class LogicalNot(plan: ExecutionPlan) extends ExecutionPlan

  final case class NumericOperation(
    operation: Numeric.Operation,
    left: ExecutionPlan,
    right: ExecutionPlan,
    numeric: DynamicValue,
  ) extends ExecutionPlan

  final case class Combine(left: ExecutionPlan, right: ExecutionPlan, o1: SchemaAst, o2: SchemaAst)
      extends ExecutionPlan

  final case class IfElse(cond: ExecutionPlan, ifTrue: ExecutionPlan, ifFalse: ExecutionPlan) extends ExecutionPlan

  final case class Pipe(first: ExecutionPlan, second: ExecutionPlan) extends ExecutionPlan

  final case class GetPath(path: List[String]) extends ExecutionPlan

  final case class SetPath(path: List[String]) extends ExecutionPlan

  final case class Equals(left: ExecutionPlan, right: ExecutionPlan) extends ExecutionPlan

  final case class FromMap(value: Map[DynamicValue, DynamicValue]) extends ExecutionPlan

  final case class Constant(value: DynamicValue) extends ExecutionPlan

  case object Identity extends ExecutionPlan
}
