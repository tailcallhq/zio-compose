package compose.interpreter

import compose.operation.OpticalOp
import zio.schema.DynamicValue
import zio.{Task, ZIO}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

trait OpticalInterpreter {
  self: Interpreter.InMemoryInterpreter =>
  def evalOptical(input: DynamicValue, operation: OpticalOp): Task[DynamicValue] = {
    operation match {
      case OpticalOp.GetPath(path) =>
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
                      else
                        loop(tail, v.asInstanceOf[DynamicValue.Record].values)
                  }
              }
            }

            ZIO.fromEither(loop(path, values))
          case _                           => ZIO.fail(new Exception("Select only works on records"))
        }

      case OpticalOp.SetPath(path) =>
        input match {
          case DynamicValue.Tuple(DynamicValue.Record(values), input) =>
            def loop(
              path: List[String],
              values: ListMap[String, DynamicValue],
              a: DynamicValue,
            ): Either[Exception, DynamicValue] = {
              path match {
                case Nil          => Left(new Exception("Path not found"))
                case head :: tail =>
                  values.get(head) match {
                    case None    => Left(new Exception("Path not found"))
                    case Some(v) =>
                      if (tail.isEmpty) Right(DynamicValue.Record(values + (head -> a)))
                      else
                        loop(tail, v.asInstanceOf[DynamicValue.Record].values, a) map { value =>
                          DynamicValue.Record(values + (head -> value))
                        }
                  }
              }
            }

            ZIO.fromEither(loop(path, values, input))
          case input => ZIO.fail(new Exception(s"Set path doesn't work on: ${input}"))
        }
    }
  }
}
