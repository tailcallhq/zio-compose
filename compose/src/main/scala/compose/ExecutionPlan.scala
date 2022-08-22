package compose

import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.{Chunk, ZIO}

sealed trait ExecutionPlan { self =>
  final def binary: Chunk[Byte] = JsonCodec.encode(ExecutionPlan.schema)(self)
  final def json: String        = new String(binary.toArray)
}

object ExecutionPlan {

  def from(json: String): ZIO[Any, Exception, ExecutionPlan] =
    from(Chunk.fromArray(json.getBytes))

  def from(chunk: Chunk[Byte]): ZIO[Any, Exception, ExecutionPlan] =
    JsonCodec.decode(ExecutionPlan.schema)(chunk) match {
      case Left(value)  => ZIO.fail(new Exception(value))
      case Right(value) => ZIO.succeed(value)
    }

  implicit def schema: Schema[ExecutionPlan] = DeriveSchema.gen[ExecutionPlan]

  final case class Scoped(operation: Scoped.Operation) extends ExecutionPlan
  object Scoped {
    sealed trait Operation
    final case class SetScope(refId: RefId, ctxId: ContextId)                      extends Operation
    final case class GetScope(refId: RefId, ctxId: ContextId, value: DynamicValue) extends Operation
    final case class WithinScope(plan: ExecutionPlan, ctxId: ContextId)            extends Operation

    final case class ContextId(id: Int)
    final case class RefId(id: Int, contextId: ContextId)
  }

  final case class Logical(operation: Logical.Operation) extends ExecutionPlan
  object Logical {
    sealed trait Operation
    final case class And(left: ExecutionPlan, right: ExecutionPlan)                              extends Operation
    final case class Or(left: ExecutionPlan, right: ExecutionPlan)                               extends Operation
    final case class Not(plan: ExecutionPlan)                                                    extends Operation
    final case class Equals(left: ExecutionPlan, right: ExecutionPlan)                           extends Operation
    final case class Diverge(cond: ExecutionPlan, ifTrue: ExecutionPlan, ifFalse: ExecutionPlan) extends Operation
  }

  final case class Numeric(operation: Numeric.Operation, kind: Numeric.Kind) extends ExecutionPlan
  object Numeric {
    sealed trait Operation
    sealed trait Kind

    object Kind {
      case object IntNumber extends Kind
    }

    final case class Add(left: ExecutionPlan, right: ExecutionPlan)                extends Operation
    final case class Multiply(left: ExecutionPlan, right: ExecutionPlan)           extends Operation
    final case class Divide(left: ExecutionPlan, right: ExecutionPlan)             extends Operation
    final case class GreaterThan(left: ExecutionPlan, right: ExecutionPlan)        extends Operation
    final case class GreaterThanEqualTo(left: ExecutionPlan, right: ExecutionPlan) extends Operation
    final case class Negate(plan: ExecutionPlan)                                   extends Operation
  }

  final case class Textual(operation: Textual.Operation) extends ExecutionPlan
  object Textual {
    sealed trait Operation
    final case class StartsWith(self: ExecutionPlan, other: ExecutionPlan) extends Operation
    final case class EndsWith(self: ExecutionPlan, other: ExecutionPlan)   extends Operation
    final case class Contains(self: ExecutionPlan, other: ExecutionPlan)   extends Operation
    final case class Length(self: ExecutionPlan)                           extends Operation
    final case class UpperCase(self: ExecutionPlan)                        extends Operation
    final case class LowerCase(self: ExecutionPlan)                        extends Operation
    final case class Concat(self: ExecutionPlan, other: ExecutionPlan)     extends Operation
  }

  final case class Optical(operation: Optical.Operation) extends ExecutionPlan
  object Optical {
    sealed trait Operation
    final case class GetPath(path: List[String]) extends Operation
    final case class SetPath(path: List[String]) extends Operation
  }

  final case class Arrow(operation: Arrow.Operation) extends ExecutionPlan
  object Arrow {
    sealed trait Operation
    final case class Zip(left: ExecutionPlan, right: ExecutionPlan)    extends Operation
    final case class Pipe(first: ExecutionPlan, second: ExecutionPlan) extends Operation
    case object Identity                                               extends Operation
  }

  final case class Debugger(operation: Debugger.Operation) extends ExecutionPlan
  object Debugger {
    sealed trait Operation
    final case class Debug(plan: ExecutionPlan, name: String) extends Operation
    final case class Show(plan: ExecutionPlan, name: String)  extends Operation
  }

  final case class Tupled(operation: Tupled.Operation) extends ExecutionPlan
  object Tupled {
    sealed trait Operation
    final case class Arg(plan: ExecutionPlan, n: Int) extends Operation
  }

  final case class Sources(operation: Sources.Operation) extends ExecutionPlan
  object Sources {
    sealed trait Operation
    final case class Default(value: DynamicValue)                    extends Operation
    final case class FromMap(value: Map[DynamicValue, DynamicValue]) extends Operation
    final case class Constant(value: DynamicValue)                   extends Operation
    final case object WriteLine                                      extends Operation
  }

  final case class Recursive(operation: Recursive.Operation) extends ExecutionPlan
  object Recursive {
    sealed trait Operation
    final case class RepeatWhile(self: ExecutionPlan, cond: ExecutionPlan) extends Operation
    final case class DoWhile(plan: ExecutionPlan, cond: ExecutionPlan)     extends Operation
  }
}
