package compose.graphql.ast

import zio.Chunk
import zio.parser.Syntax

final case class OperationDefinition(
  operation: OperationDefinition.Operation,
  name: Option[String],
  selectionSet: Chunk[OperationDefinition.Field],
)

object OperationDefinition {

  /**
   * Classes Section
   */

  sealed trait Operation
  case object QueryOperation        extends Operation
  case object MutationOperation     extends Operation
  case object SubscriptionOperation extends Operation

  final case class Field(
    name: String,
    arguments: Chunk[Argument] = Chunk.empty,
    selection: Chunk[Field] = Chunk.empty,
  )

  final case class Argument(name: String, value: Value)

  sealed trait Value
  final case class IntValue(value: Int)         extends Value
  final case class StringValue(value: String)   extends Value
  final case class BooleanValue(value: Boolean) extends Value
  final case class VariableValue(name: String)  extends Value

  /**
   * Methods Section
   */

  private lazy val emptySpace  = (Syntax.char('\n') | Syntax.char(' ')).repeat.unit(Chunk {})
  private lazy val emptySpace0 = (Syntax.char('\n') | Syntax.char(' ')).repeat0.unit(Chunk {})

  private lazy val anyName = Syntax.letter.repeat
    .transform[String](_.mkString, i => Chunk.fromArray(i.toCharArray()))

  private lazy val booleanValue = (Syntax.string("true", true) | Syntax.string("false", false))
    .transform[BooleanValue](BooleanValue, _.value)

  private lazy val intValue = Syntax.digit.repeat.transform[IntValue](
    digits => IntValue(digits.mkString("").toInt),
    i => Chunk.fromArray(i.value.toString.toCharArray),
  )

  private lazy val stringValue = anyName.between(Syntax.char('"'), Syntax.char('"'))
    .transform[StringValue](StringValue, _.value)

  private lazy val variableValue = Syntax.char('$') ~ anyName
    .transform[VariableValue](VariableValue, _.name)

  private lazy val value: GraphQLSyntax[Value] = booleanValue.widen[Value] | intValue
    .widen[Value] | stringValue.widen[Value] | variableValue.widen[Value]

  private lazy val arguments: GraphQLSyntax[Chunk[Argument]] = (anyName ~ emptySpace0 ~ Syntax
    .char(':') ~ emptySpace0 ~ value).repeatWithSep0(Syntax.char(',')).transform[Chunk[Argument]](
    i => i.map(i => Argument(i._1, i._2)),
    a => a.map(a => (a.name, a.value)),
  ).between(Syntax.char('('), Syntax.char(')'))

  private lazy val nestedField: GraphQLSyntax[Field] = {
    anyName ~ emptySpace0 ~ arguments.optional ~ emptySpace0 ~ fieldSyntax
  }.transform[Field](
    { case (string, arg, chunk) => Field(string, arg.getOrElse(Chunk.empty), chunk) },
    { field => (field.name, Some(field.arguments), field.selection) },
  )

  private lazy val leafField: GraphQLSyntax[Field] = { anyName }
    .transform[Field]({ case (string) => Field(string, Chunk.empty) }, _.name)

  private lazy val fieldSyntax: GraphQLSyntax[Chunk[Field]] = (nestedField | leafField)
    .repeatWithSep0(emptySpace).surroundedBy(emptySpace0)
    .between(Syntax.char('{'), Syntax.char('}'))

  lazy val syntax: GraphQLSyntax[OperationDefinition] = (Syntax
    .string("query", {}) ~ emptySpace ~ fieldSyntax).transform[OperationDefinition](
    { fields => OperationDefinition(QueryOperation, None, fields) },
    { operation => operation.selectionSet },
  )
}
