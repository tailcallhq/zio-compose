package com.tusharmath.compose

import zio.{Task, ZIO}
import zio.schema.DynamicValue

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Executor {
  def execute(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] =
    plan match {
      case ExecutionPlan.Combine(left, right)    =>
        execute(left, input).zip(execute(right, input)) map { case (d1, d2) => DynamicValue.Tuple(d1, d2) }
      case ExecutionPlan.Constant(value)         => ZIO.succeed(value)
      case ExecutionPlan.Sequence(first, second) => execute(first, input).flatMap(output => execute(second, output))
      case ExecutionPlan.Identity                => ZIO.succeed(input)
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
    }
}
