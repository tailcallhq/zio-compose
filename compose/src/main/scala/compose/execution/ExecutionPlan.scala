package compose.execution

import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.{Chunk, ZIO}

sealed trait ExecutionPlan { self =>
  def binary: Chunk[Byte] = JsonCodec.encode(ExecutionPlan.schema)(self)

  def json: String = new String(binary.toArray)
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

  case class ScopeOperation(operation: ScopeOperation.Operation) extends ExecutionPlan
  object ScopeOperation {

    sealed trait Operation
    final case class SetScope(scopeId: ScopeId, ctxId: ContextId)                      extends Operation
    final case class GetScope(scopeId: ScopeId, ctxId: ContextId, value: DynamicValue) extends Operation
    final case class WithinScope(plan: ExecutionPlan, ctxId: ContextId)                extends Operation

    case class ContextId(id: Int)
    case class ScopeId(id: Int, contextId: ContextId)
  }

  final case class LogicalOperation(operation: LogicalOperation.Operation) extends ExecutionPlan

  object LogicalOperation {
    sealed trait Operation
    final case class And(left: ExecutionPlan, right: ExecutionPlan)                              extends Operation
    final case class Or(left: ExecutionPlan, right: ExecutionPlan)                               extends Operation
    final case class Not(plan: ExecutionPlan)                                                    extends Operation
    final case class Equals(left: ExecutionPlan, right: ExecutionPlan)                           extends Operation
    final case class Diverge(cond: ExecutionPlan, ifTrue: ExecutionPlan, ifFalse: ExecutionPlan) extends Operation
  }

  final case class NumericOperation(operation: NumericOperation.Operation, kind: NumericOperation.Kind)
      extends ExecutionPlan

  object NumericOperation {
    sealed trait Kind

    object Kind {
      case object IntNumber extends Kind
    }

    sealed trait Operation
    case class Add(left: ExecutionPlan, right: ExecutionPlan)                extends Operation
    case class Multiply(left: ExecutionPlan, right: ExecutionPlan)           extends Operation
    case class Divide(left: ExecutionPlan, right: ExecutionPlan)             extends Operation
    case class GreaterThan(left: ExecutionPlan, right: ExecutionPlan)        extends Operation
    case class GreaterThanEqualTo(left: ExecutionPlan, right: ExecutionPlan) extends Operation
    case class Negate(plan: ExecutionPlan)                                   extends Operation
  }

  case class StringOperation(operation: StringOperation.Operation) extends ExecutionPlan

  object StringOperation {
    sealed trait Operation
    case class StartsWith(self: ExecutionPlan, other: ExecutionPlan) extends Operation
    case class EndsWith(self: ExecutionPlan, other: ExecutionPlan)   extends Operation
    case class Contains(self: ExecutionPlan, other: ExecutionPlan)   extends Operation
    case class Length(self: ExecutionPlan)                           extends Operation
    case class UpperCase(self: ExecutionPlan)                        extends Operation
    case class LowerCase(self: ExecutionPlan)                        extends Operation
  }

  final case class Arg(plan: ExecutionPlan, n: Int) extends ExecutionPlan

  final case class RepeatWhile(self: ExecutionPlan, cond: ExecutionPlan) extends ExecutionPlan

  final case class Concat(self: ExecutionPlan, other: ExecutionPlan, canConcat: DynamicValue) extends ExecutionPlan

  final case class Default(value: DynamicValue) extends ExecutionPlan

  final case class Zip(left: ExecutionPlan, right: ExecutionPlan) extends ExecutionPlan

  final case class Pipe(first: ExecutionPlan, second: ExecutionPlan) extends ExecutionPlan

  final case class GetPath(path: List[String]) extends ExecutionPlan

  final case class SetPath(path: List[String]) extends ExecutionPlan

  final case class FromMap(value: Map[DynamicValue, DynamicValue]) extends ExecutionPlan

  final case class Constant(value: DynamicValue) extends ExecutionPlan

  final case class Debug(plan: ExecutionPlan, name: String) extends ExecutionPlan

  final case class Show(plan: ExecutionPlan, name: String) extends ExecutionPlan

  final case class DoWhile(plan: ExecutionPlan, cond: ExecutionPlan) extends ExecutionPlan

  case object Identity extends ExecutionPlan
}
