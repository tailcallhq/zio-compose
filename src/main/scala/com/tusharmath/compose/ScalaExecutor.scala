package com.tusharmath.compose

import com.tusharmath.compose.internal.DParse
import zio.{Task, ZIO}
import zio.schema.{DynamicValue, Schema}
import zio.schema.ast.SchemaAst

object ScalaExecutor {

  def evalAsBoolean[A](input: DynamicValue)(
    f: (Boolean, Boolean) => A,
  )(implicit ev: Schema[A]): Either[String, DynamicValue] = {
    DParse.toBooleanTuple(input).map { case (v1, v2) => encode(f(v1, v2)) }
  }

  def evalAsInt[A](input: DynamicValue)(
    f: (Int, Int) => A,
  )(implicit ev: Schema[A]): Either[String, DynamicValue] = {
    DParse.toIntTuple(input).map { case (v1, v2) => encode(f(v1, v2)) }
  }

  def execute(plan: ExecutionPlan, dv: DynamicValue): Task[DynamicValue] = {
    plan match {
      case ExecutionPlan.Literal(a)       => ZIO.succeed(a)
      case ExecutionPlan.AddInteger(a, b) =>
        for {
          a <- a.executeWith(dv).flatMap(x => effect(DParse.toInt(x)))
          b <- b.executeWith(dv).flatMap(x => effect(DParse.toInt(x)))
        } yield encode(a + b)

      case ExecutionPlan.IfElse(cond, ifTrue, ifFalse) =>
        for {
          cond    <- cond
            .executeWith(dv)
            .flatMap(x => effect(DParse.toBoolean(x)))
          ifTrue  <- ifTrue.executeWith(dv)
          ifFalse <- ifFalse.executeWith(dv)
        } yield if (cond) ifTrue else ifFalse

      case ExecutionPlan.GreaterThan(first, second) =>
        for {
          i <- first.executeWith(dv).flatMap(x => effect(DParse.toInt(x)))
          j <- second.executeWith(dv).flatMap(x => effect(DParse.toInt(x)))
        } yield encode(i > j)

      case ExecutionPlan.ExecMap(input, f) =>
        for {
          dv     <- input.executeWith(dv)
          result <- f.executeWith(dv)
        } yield result
      case ExecutionPlan.Length            =>
        effect(DParse.toString(dv).map(str => encode(str.length)))
      case ExecutionPlan.UpperCase         =>
        effect(DParse.toString(dv).map(str => encode(str.toUpperCase)))

      case ExecutionPlan.And(left, right) =>
        for {
          left  <- left.execute.flatMap(dv => effect(DParse.toBoolean(dv)))
          right <- right.execute.flatMap(dv => effect(DParse.toBoolean(dv)))
        } yield encode(left && right)

    }
  }

  private def effect[A](e: Either[String, A]): Task[A] =
    e match {
      case Left(error) => ZIO.fail(new Exception(error))
      case Right(a)    => ZIO.succeed(a)
    }

  private def encode[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)

  private def merge(
    ds1: (DynamicValue, SchemaAst),
    ds2: (DynamicValue, SchemaAst),
  ): Either[String, DynamicValue] = {
    val s1 = ds1._2.toSchema.asInstanceOf[Schema[Any]]
    val s2 = ds2._2.toSchema.asInstanceOf[Schema[Any]]
    val d1 = ds1._1
    val d2 = ds2._1
    for {
      v1 <- d1.toTypedValue(s1)
      v2 <- d2.toTypedValue(s2)
    } yield (s1 <*> s2).toDynamic((v1, v2))
  }

}
