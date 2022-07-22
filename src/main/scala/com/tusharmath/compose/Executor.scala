package com.tusharmath.compose

import com.tusharmath.compose.internal.{DExtract, DParse}
import zio.{Task, ZIO}
import zio.prelude.AssociativeBothOps
import zio.schema.{DynamicValue, Schema}
import zio.schema.ast.SchemaAst

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Executor {

  def execute(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
    plan match {
      case ExecutionPlan.Zip2(f1, f2, o1, o2) =>
        for {
          d1a <- effect(DExtract.paramIdN(1, input))
          d2a <- effect(DExtract.paramIdN(2, input))
          res <- f1.unsafeExecute(d1a).zipPar(f2.unsafeExecute(d2a)) flatMap {
            case (d1b, d2b) => effect(merge((d1b, o1), (d2b, o2)))
          }
        } yield res

      case ExecutionPlan.Always(value)           => ZIO.succeed(value)
      case ExecutionPlan.Sequence(first, second) =>
        first.unsafeExecute(input).flatMap(second.unsafeExecute)
      case ExecutionPlan.Identity                => ZIO.succeed(input)
      case ExecutionPlan.AddInt                  =>
        effect(evalAsInt(input) { (v1, v2) => v1 + v2 })

      case ExecutionPlan.MulInt =>
        effect(evalAsInt(input) { (v1, v2) => v1 * v2 })

      case ExecutionPlan.Dictionary(value) =>
        value.get(input) match {
          case Some(v) => ZIO.succeed(v)
          case None    =>
            ZIO.fail(new Exception("Key lookup failed in dictionary"))
        }
      case ExecutionPlan.Select(path)      =>
        input match {
          case DynamicValue.Record(values) =>
            @tailrec
            def loop(
              path: List[String],
              values: ListMap[String, DynamicValue],
            ): Either[Exception, DynamicValue] = {
              path match {
                case Nil          => Left(new Exception("Path not found"))
                case head :: tail =>
                  values.get(head) match {
                    case None    => Left(new Exception("Path not found"))
                    case Some(v) =>
                      if (tail.isEmpty) Right(v)
                      else
                        loop(tail, v.asInstanceOf[DynamicValue.Record].values)
                  }
              }
            }

            ZIO.fromEither(loop(path, values))

          case _ => ZIO.fail(new Exception("Select only works on records"))
        }

      case ExecutionPlan.Partial(argSchemas, argValues) =>
        effect {
          argValues.appended(input).zip(argSchemas).toArray match {
            case Array(da1, da2) => merge(da1, da2)
          }
        }

      case ExecutionPlan.IfElse(cond, isTrue, isFalse) =>
        for {
          dBool   <- cond.unsafeExecute(input)
          bool    <- effect(DParse.toBoolean(dBool))
          dResult <-
            if (bool) isTrue.unsafeExecute(input)
            else isFalse.unsafeExecute(input)
        } yield dResult

      case ExecutionPlan.LogicalNot =>
        effect(DParse.toBoolean(input).map(bool => encode(!bool)))

      case ExecutionPlan.EqualTo =>
        val p1 = DExtract.paramIdN(1, input)
        val p2 = DExtract.paramIdN(2, input)
        effect(p1.zip(p2).map(encode(_)))

      case ExecutionPlan.GreaterThanInt =>
        effect(evalAsInt(input) { (v1, v2) => v1 > v2 })

      case ExecutionPlan.GreaterThanEqualInt =>
        effect(evalAsInt(input) { (v1, v2) => v1 >= v2 })

      case ExecutionPlan.LogicalAnd =>
        effect(evalAsBoolean(input) { (v1, v2) => v1 && v2 })

      case ExecutionPlan.LogicalOr =>
        effect(evalAsBoolean(input) { (v1, v2) => v1 || v2 })

      case ExecutionPlan.Converge(f, f1, f2, a1, a2) =>
        for {
          d1 <- f1.unsafeExecute(input)
          d2 <- f2.unsafeExecute(input)
          s1 = a1.toSchema.asInstanceOf[Schema[Any]]
          s2 = a2.toSchema.asInstanceOf[Schema[Any]]
          v1  <- effect(d1.toTypedValue(s1))
          v2  <- effect(d2.toTypedValue(s2))
          res <- f.unsafeExecute((s1 <*> s2).toDynamic((v1, v2)))
        } yield res
    }
  }

  def evalAsInt[A](input: DynamicValue)(
    f: (Int, Int) => A,
  )(implicit ev: Schema[A]): Either[String, DynamicValue] = {
    DParse.toIntTuple(input).map { case (v1, v2) => encode(f(v1, v2)) }
  }

  private def encode[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)

  def evalAsBoolean[A](input: DynamicValue)(
    f: (Boolean, Boolean) => A,
  )(implicit ev: Schema[A]): Either[String, DynamicValue] = {
    DParse.toBooleanTuple(input).map { case (v1, v2) => encode(f(v1, v2)) }
  }

  private def effect[A](e: Either[String, A]): Task[A] =
    e match {
      case Left(error) => ZIO.fail(new Exception(error))
      case Right(a)    => ZIO.succeed(a)
    }

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
