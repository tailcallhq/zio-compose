package compose.graphql.ast

/**
 * GraphQL is the AST used to represent a GraphQL query,
 * mutation, subscription or schema. The AST can be
 * generated in two ways viz. from a string. and from a list
 * of connections.
 */
sealed trait Ast {
  self =>
  final def encode: String = AstPrinter.render(self)
}

object Ast {
  final case class Document(definitions: Seq[Definitions]) extends Ast

  sealed trait Definitions extends Ast

  final case class ObjectType(name: String, fields: List[Field]) extends Definitions

  final case class Field(name: String, arguments: List[InputValue], fieldType: FieldType)
      extends Ast

  sealed trait FieldType extends Ast
  object FieldType {
    final case class NamedFieldType(name: String)      extends FieldType
    final case class RequiredFieldType(tpe: FieldType) extends FieldType
    final case class ListFieldType(tpe: FieldType)     extends FieldType
  }

  final case class InputValue(name: String, fieldType: FieldType) extends Ast
}
