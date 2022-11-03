package compose.graphql.ast

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
    final case class ObjectType(name: String, fields: Seq[Field]) extends Definitions
    final case class Field(name: String, arguments: Seq[InputValue], fieldType: Type)
        extends Definitions
    final case class InputValue(name: String, fieldType: Type)    extends Definitions
  }

  sealed trait Type extends Ast
  object Type {
    final case class Named(name: String) extends Type
    final case class NotNull(tpe: Type)  extends Type
    final case class List(tpe: Type)     extends Type
  }
}
