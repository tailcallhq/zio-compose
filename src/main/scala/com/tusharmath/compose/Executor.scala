package com.tusharmath.compose

import zio.{Task, ZIO}
import zio.schema.{DynamicValue, Schema}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Executor {

  def execute(plan: ExecutionPlan, input: DynamicValue): Task[DynamicValue] = {
    plan match {
      case ExecutionPlan.Zip2(f1, f2, i1, i2, o1, o2) =>
        val iSchema1 = i1.toSchema.asInstanceOf[Schema[Any]]
        val iSchema2 = i2.toSchema.asInstanceOf[Schema[Any]]
        val oSchema1 = o1.toSchema.asInstanceOf[Schema[Any]]
        val oSchema2 = o2.toSchema.asInstanceOf[Schema[Any]]
        val si       = Schema.tuple2(iSchema1, iSchema2)

        input.toTypedValue(si) match {
          case Left(error)  => ZIO.fail(new Exception(error))
          case Right(value) =>
            f1.unsafeExecute(iSchema1.toDynamic(value._1))
              .zipWithPar(f2.unsafeExecute(iSchema2.toDynamic(value._2))) {
                case (b1, b2) =>
                  effect {
                    for {
                      v1 <- b1.toTypedValue(oSchema1)
                      v2 <- b2.toTypedValue(oSchema2)
                    } yield (oSchema1 <*> oSchema2).toDynamic((v1, v2))
                  }
              }
              .flatten
        }

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
            case Array(d1 -> a1, d2 -> a2) =>
              val s1 = a1.toSchema.asInstanceOf[Schema[Any]]
              val s2 = a2.toSchema.asInstanceOf[Schema[Any]]

              for {
                v1 <- d1.toTypedValue(s1)
                v2 <- d2.toTypedValue(s2)
              } yield (s1 <*> s2).toDynamic((v1, v2))
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

}
