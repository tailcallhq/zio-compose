package compose.graphql

/**
 * GraphQL is the AST used to represent a GraphQL query,
 * mutation, subscription or schema. The AST can be
 * generated in two ways viz. from a string. and from a list
 * of connections.
 */
sealed trait Ast

object Ast {
  final case class Document(definitions: Seq[Definitions]) extends Ast

  sealed trait Definitions extends Ast {
    def name: String
  }

  object Definitions {
    final case class ObjectTypeDefinition(name: String, fields: Seq[FieldDefinition])
        extends Definitions
    final case class FieldDefinition(
      name: String,
      arguments: Seq[InputValueDefinition],
      fieldType: Type,
    ) extends Definitions
    final case class InputValueDefinition(name: String, fieldType: Type) extends Definitions
  }

  sealed trait Type extends Ast
  object Type {
    final case class NamedType(name: String) extends Type
    final case class NotNullType(tpe: Type)  extends Type
    final case class ListType(tpe: Type)     extends Type
  }
}
