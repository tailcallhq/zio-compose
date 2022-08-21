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

  final case class ScopeExecution(operation: ScopeExecution.Operation) extends ExecutionPlan
  object ScopeExecution {
    sealed trait Operation
    final case class SetScope(refId: RefId, ctxId: ContextId)                      extends Operation
    final case class GetScope(refId: RefId, ctxId: ContextId, value: DynamicValue) extends Operation
    final case class WithinScope(plan: ExecutionPlan, ctxId: ContextId)            extends Operation

    final case class ContextId(id: Int)
    final case class RefId(id: Int, contextId: ContextId)
  }

  final case class LogicalExecution(operation: LogicalExecution.Operation) extends ExecutionPlan
  object LogicalExecution {
    sealed trait Operation
    final case class And(left: ExecutionPlan, right: ExecutionPlan)                              extends Operation
    final case class Or(left: ExecutionPlan, right: ExecutionPlan)                               extends Operation
    final case class Not(plan: ExecutionPlan)                                                    extends Operation
    final case class Equals(left: ExecutionPlan, right: ExecutionPlan)                           extends Operation
    final case class Diverge(cond: ExecutionPlan, ifTrue: ExecutionPlan, ifFalse: ExecutionPlan) extends Operation
  }

  final case class NumericExecution(operation: NumericExecution.Operation, kind: NumericExecution.Kind)
      extends ExecutionPlan
  object NumericExecution {
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

  final case class StringExecution(operation: StringExecution.Operation) extends ExecutionPlan
  object StringExecution {
    sealed trait Operation
    final case class StartsWith(self: ExecutionPlan, other: ExecutionPlan) extends Operation
    final case class EndsWith(self: ExecutionPlan, other: ExecutionPlan)   extends Operation
    final case class Contains(self: ExecutionPlan, other: ExecutionPlan)   extends Operation
    final case class Length(self: ExecutionPlan)                           extends Operation
    final case class UpperCase(self: ExecutionPlan)                        extends Operation
    final case class LowerCase(self: ExecutionPlan)                        extends Operation
    final case class Concat(self: ExecutionPlan, other: ExecutionPlan)     extends Operation
  }

  final case class OpticalExecution(operation: OpticalExecution.Operation) extends ExecutionPlan
  object OpticalExecution {
    sealed trait Operation
    final case class GetPath(path: List[String]) extends Operation
    final case class SetPath(path: List[String]) extends Operation
  }

  final case class ArrowExecution(operation: ArrowExecution.Operation) extends ExecutionPlan
  object ArrowExecution {
    sealed trait Operation
    final case class Zip(left: ExecutionPlan, right: ExecutionPlan)    extends Operation
    final case class Pipe(first: ExecutionPlan, second: ExecutionPlan) extends Operation
    case object Identity                                               extends Operation
  }

  final case class DebugExecution(operation: DebugExecution.Operation) extends ExecutionPlan
  object DebugExecution {
    sealed trait Operation
    final case class Debug(plan: ExecutionPlan, name: String) extends Operation
    final case class Show(plan: ExecutionPlan, name: String)  extends Operation
  }

  final case class TupleExecution(operation: TupleExecution.Operation) extends ExecutionPlan
  object TupleExecution {
    sealed trait Operation
    final case class Arg(plan: ExecutionPlan, n: Int) extends Operation
  }

  final case class SourceExecution(operation: SourceExecution.Operation) extends ExecutionPlan
  object SourceExecution {
    sealed trait Operation
    final case class Default(value: DynamicValue)                    extends Operation
    final case class FromMap(value: Map[DynamicValue, DynamicValue]) extends Operation
    final case class Constant(value: DynamicValue)                   extends Operation
    final case object WriteLine                                      extends Operation
  }

  final case class RecursiveExecution(operation: RecursiveExecution.Operation) extends ExecutionPlan
  object RecursiveExecution {
    sealed trait Operation
    final case class RepeatWhile(self: ExecutionPlan, cond: ExecutionPlan) extends Operation
    final case class DoWhile(plan: ExecutionPlan, cond: ExecutionPlan)     extends Operation
  }
}
