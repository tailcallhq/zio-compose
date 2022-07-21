package com.tusharmath.compose

import zio.{Task, ZIO}
import zio.schema.{DynamicValue, Schema}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Executor {

  def execute(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
    plan match {
      case ExecutionPlan.Zip2(f1, f2, ast1, ast2) =>
        val s1 = ast1.toSchema.asInstanceOf[Schema[Any]]
        val s2 = ast2.toSchema.asInstanceOf[Schema[Any]]

        execute(f1, input)
          .zipWithPar(execute(f2, input)) { (d1, d2) =>
            ZIO
              .fromEither(for {
                v1 <- d1.toTypedValue(s1)
                v2 <- d2.toTypedValue(s2)
              } yield (s1 <*> s2).toDynamic((v1, v2)))
              .catchAll(err => ZIO.fail(new Exception(err)))
          }
          .flatten

      case ExecutionPlan.Constant(value)         => ZIO.succeed(value)
      case ExecutionPlan.Sequence(first, second) => execute(first, input).flatMap(output => execute(second, output))
      case ExecutionPlan.Identity                => ZIO.succeed(input)
      case ExecutionPlan.AddInt                  =>
        input.toTypedValue(Schema[(Int, Int)]) match {
          case Left(error)   => ZIO.fail(new Exception(error))
          case Right(a -> b) => ZIO.succeed(Schema.primitive[Int].toDynamic(a + b))
        }
      case ExecutionPlan.Dictionary(value)       =>
        value.get(input) match {
          case Some(v) => ZIO.succeed(v)
          case None    => ZIO.fail(new Exception("Key lookup failed in dictionary"))
        }
      case ExecutionPlan.Select(path)            =>
        input match {
          case DynamicValue.Record(values) =>
            @tailrec
            def loop(path: List[String], values: ListMap[String, DynamicValue]): Either[Exception, DynamicValue] = {
              path match {
                case Nil          => Left(new Exception("Path not found"))
                case head :: tail =>
                  values.get(head) match {
                    case None    => Left(new Exception("Path not found"))
                    case Some(v) =>
                      if (tail.isEmpty) Right(v)
                      else loop(tail, v.asInstanceOf[DynamicValue.Record].values)
                  }
              }
            }

            ZIO.fromEither(loop(path, values))

          case _ => ZIO.fail(new Exception("Select only works on records"))
        }

      case ExecutionPlan.Partial(plan, argSchema, args) =>
        ZIO.fromEither {
          val tuples = args.appended(input).zip(argSchema)
          tuples.toArray match {
            case Array(d1 -> a1, d2 -> a2) =>
              val (s1, s2) = (a1.toSchema.asInstanceOf[Schema[Any]], a2.toSchema.asInstanceOf[Schema[Any]])
              for {
                v1 <- d1.toTypedValue(s1)
                v2 <- d2.toTypedValue(s2)
              } yield (s1 <*> s2).toDynamic((v1, v2))
          }
        }.catchAll(err => ZIO.fail(new Exception(err))).flatMap(plan.unsafeExecute(_))
    }
  }
}
