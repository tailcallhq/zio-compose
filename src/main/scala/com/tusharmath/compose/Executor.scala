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
                  ZIO.fromEither {
                    for {
                      v1 <- b1.toTypedValue(oSchema1)
                      v2 <- b2.toTypedValue(oSchema2)
                    } yield (oSchema1 <*> oSchema2).toDynamic((v1, v2))
                  }.catchAll(err => ZIO.fail(new Exception(err)))
              }
              .flatten
        }

      case ExecutionPlan.Constant(value)         => ZIO.succeed(value)
      case ExecutionPlan.Sequence(first, second) =>
        first.unsafeExecute(input).flatMap(second.unsafeExecute)
      case ExecutionPlan.Identity                => ZIO.succeed(input)
      case ExecutionPlan.AddInt                  =>
        getParams2[Int](input) match {
          case Left(error)   => ZIO.fail(new Exception(error))
          case Right(a -> b) =>
            ZIO.succeed(Schema.primitive[Int].toDynamic(a + b))
        }

      case ExecutionPlan.MulInt            =>
        getParams2[Int](input) match {
          case Left(error)   => ZIO.fail(new Exception(error))
          case Right(a -> b) =>
            ZIO.succeed(Schema.primitive[Int].toDynamic(a * b))
        }
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

      case ExecutionPlan.Partial(plan, argSchema, args) =>
        ZIO.fromEither {
          val tuples = args.appended(input).zip(argSchema)
          tuples.toArray match {
            case Array(d1 -> a1, d2 -> a2) =>
              val (s1, s2) = (
                a1.toSchema.asInstanceOf[Schema[Any]],
                a2.toSchema.asInstanceOf[Schema[Any]],
              )
              for {
                v1 <- d1.toTypedValue(s1)
                v2 <- d2.toTypedValue(s2)
              } yield (s1 <*> s2).toDynamic((v1, v2))
          }
        }.catchAll(err => ZIO.fail(new Exception(err)))
          .flatMap(plan.unsafeExecute)
    }
  }

  def getParams2[A](
    db: DynamicValue,
  )(implicit s: Schema[A]): Either[String, (A, A)] =
    (db match {
      case DynamicValue.Record(values)     =>
        values
          .get("_1")
          .zip(values.get("_2"))
          .toRight("Record isn't like a tuple")
      case DynamicValue.Tuple(left, right) => Right((left, right))
      case _                               => Left("Not a tuple")
    }).flatMap { case (d1, d2) =>
      d1
        .toTypedValue(Schema[A])
        .flatMap(v1 => d2.toTypedValue(Schema[A]).map(v2 => (v1, v2)))
    }

}
