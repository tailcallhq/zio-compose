package compose

import zio.schema.{DeriveSchema, DynamicValue, Schema}
import zio.schema.codec.JsonCodec
import zio.{Chunk, ZIO}
import zio.schema.ast.SchemaAst

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

  sealed trait Scoped extends ExecutionPlan
  object Scoped {
    final case class SetScope(refId: RefId, ctxId: ContextId)                      extends Scoped
    final case class GetScope(refId: RefId, ctxId: ContextId, value: DynamicValue) extends Scoped
    final case class WithinScope(plan: ExecutionPlan, ctxId: ContextId)            extends Scoped
    final case class ContextId(id: Int)
    final case class RefId(id: Int, contextId: ContextId)
  }

  sealed trait Logical extends ExecutionPlan
  object Logical {
    final case class And(left: ExecutionPlan, right: ExecutionPlan)                              extends Logical
    final case class Or(left: ExecutionPlan, right: ExecutionPlan)                               extends Logical
    final case class Not(plan: ExecutionPlan)                                                    extends Logical
    final case class Equals(left: ExecutionPlan, right: ExecutionPlan)                           extends Logical
    final case class Diverge(cond: ExecutionPlan, ifTrue: ExecutionPlan, ifFalse: ExecutionPlan) extends Logical
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

  sealed trait Textual extends ExecutionPlan
  object Textual {
    final case class StartsWith(self: ExecutionPlan, other: ExecutionPlan) extends Textual
    final case class EndsWith(self: ExecutionPlan, other: ExecutionPlan)   extends Textual
    final case class Contains(self: ExecutionPlan, other: ExecutionPlan)   extends Textual
    final case class Length(self: ExecutionPlan)                           extends Textual
    final case class UpperCase(self: ExecutionPlan)                        extends Textual
    final case class LowerCase(self: ExecutionPlan)                        extends Textual
    final case class Concat(self: ExecutionPlan, other: ExecutionPlan)     extends Textual
  }

  sealed trait Optical extends ExecutionPlan
  object Optical {
    final case class GetPath(path: List[String]) extends Optical
    final case class SetPath(path: List[String]) extends Optical
  }

  sealed trait Arrow extends ExecutionPlan
  object Arrow {
    final case class Zip(left: ExecutionPlan, right: ExecutionPlan)    extends Arrow
    final case class Pipe(first: ExecutionPlan, second: ExecutionPlan) extends Arrow
    case object ToInt                                                  extends Arrow
    case object Identity                                               extends Arrow
    case class AsString(ast: SchemaAst)                                extends Arrow
  }

  sealed trait Debugger extends ExecutionPlan
  object Debugger {
    final case class Debug(plan: ExecutionPlan, name: String) extends Debugger
    final case class Show(plan: ExecutionPlan, name: String)  extends Debugger
    final case class Address(plan: ExecutionPlan)             extends Debugger
  }

  sealed trait Tupled extends ExecutionPlan
  object Tupled {
    final case class Arg(plan: ExecutionPlan, n: Int) extends Tupled
  }

  sealed trait Sources extends ExecutionPlan
  object Sources {
    final case class Default(value: DynamicValue)                    extends Sources
    final case class FromMap(value: Map[DynamicValue, DynamicValue]) extends Sources
    final case class Constant(value: DynamicValue)                   extends Sources
  }

  sealed trait Console extends ExecutionPlan
  object Console {
    case object WriteLine                             extends Console
    final case class ReadLine(prompt: Option[String]) extends Console
  }

  sealed trait Recursive extends ExecutionPlan
  object Recursive {
    final case class RecurseWhile(plan: ExecutionPlan, cond: ExecutionPlan) extends Recursive
    final case class RepeatWhile(plan: ExecutionPlan, cond: ExecutionPlan)  extends Recursive
  }

  sealed trait Fold extends ExecutionPlan
  object Fold {
    final case class FoldOption(isEmpty: ExecutionPlan, f: ExecutionPlan)  extends Fold
    final case class FoldEither(left: ExecutionPlan, right: ExecutionPlan) extends Fold
  }

  sealed trait Optional extends ExecutionPlan
  object Optional {
    case object IsEmpty extends Optional
  }

  sealed trait EitherOne extends ExecutionPlan
  object EitherOne {
    case object IsLeft extends EitherOne
  }

  sealed trait Random extends ExecutionPlan
  object Random {
    case class NextInt(min: ExecutionPlan, max: ExecutionPlan) extends Random
  }

  sealed trait Remote extends ExecutionPlan
  object Remote {
    case object Http extends Remote
  }

  sealed trait Codec extends ExecutionPlan
  object Codec {
    case object Encode                extends Codec
    case class Decode(ast: SchemaAst) extends Codec
  }
}
