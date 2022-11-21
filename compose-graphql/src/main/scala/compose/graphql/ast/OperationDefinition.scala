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
  final case class IntValue(value: Int)        extends Value
  final case class StringValue(value: String)  extends Value
  final case class VariableValue(name: String) extends Value

  /**
   * Methods Section
   */

  private lazy val emptySpace  = (Syntax.char('\n') | Syntax.char(' ')).repeat.unit(Chunk {})
  private lazy val emptySpace0 = (Syntax.char('\n') | Syntax.char(' ')).repeat0.unit(Chunk {})

  private lazy val anyName = Syntax.letter.repeat
    .transform[String](_.mkString, i => Chunk.fromArray(i.toCharArray()))

  private lazy val nestedField: GraphQLSyntax[Field] = { anyName ~ emptySpace ~ fieldSyntax }
    .transform[Field](
      { case (string, chunk) => Field(string, Chunk.empty, chunk) },
      { field => (field.name, field.selection) },
    )

  private lazy val leafField: GraphQLSyntax[Field] = { anyName }
    .transform[Field]({ case (string) => Field(string, Chunk.empty) }, { case field => field.name })

  private lazy val fieldSyntax: GraphQLSyntax[Chunk[Field]] = (nestedField | leafField)
    .repeatWithSep0(emptySpace).surroundedBy(emptySpace0)
    .between(Syntax.char('{'), Syntax.char('}'))

  lazy val syntax: GraphQLSyntax[OperationDefinition] = (Syntax
    .string("query", {}) ~ emptySpace ~ fieldSyntax).transform[OperationDefinition](
    { fields => OperationDefinition(QueryOperation, None, fields) },
    { operation => operation.selectionSet },
  )
}
