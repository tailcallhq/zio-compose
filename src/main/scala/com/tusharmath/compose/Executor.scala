package com.tusharmath.compose

import zio.{Task, ZIO}
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
        val p1 = DExtract.paramIdN(1, input).flatMap(DParse.toInt)
        val p2 = DExtract.paramIdN(2, input).flatMap(DParse.toInt)
        effect(p1.flatMap(v1 => p2.map(v2 => encode(v1 + v2))))

      case ExecutionPlan.MulInt =>
        val p1 = DExtract.paramIdN(1, input).flatMap(DParse.toInt)
        val p2 = DExtract.paramIdN(2, input).flatMap(DParse.toInt)
        effect(p1.flatMap(v1 => p2.map(v2 => encode(v1 * v2))))

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
    }
  }

  private def encode[A](a: A)(implicit schema: Schema[A]): DynamicValue =
    schema.toDynamic(a)

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
