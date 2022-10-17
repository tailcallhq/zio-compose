package compose.graphql

/**
 * GraphQL is the AST used to represent a GraphQL query,
 * mutation, subscription or schema. The AST can be
 * generated in two ways viz. from a string. and from a list
 * of connections.
 */
sealed trait Ast {
  self =>
  import Ast._
  final def encode: String = self match {
    case Ast.Document(definitions) =>
      s"\n${definitions.sortBy { case Ast.ObjectType(name, _) => name }.map(_.encode).mkString("\n")}\n"

    case definitions: Ast.Definitions => definitions match {
        case ObjectType(name, fields) => s"type $name {\n${fields.map(_.encode).mkString("\n")}\n}"
      }

    case Ast.Field(name, arguments, fieldType) =>
      val args = if (arguments.isEmpty) "" else s"(${arguments.map(_.encode).mkString(", ")})"
      val tpe  = fieldType.encode
      s"$name$args: $tpe"

    case fieldType: Ast.FieldType => fieldType match {
        case FieldType.NamedFieldType(name)   => name
        case FieldType.RequiredFieldType(tpe) => s"${tpe.encode}!"
        case FieldType.ListFieldType(tpe)     => s"[${tpe.encode}]"
      }

    case Ast.InputValue(name, fieldType) => s"$name: ${fieldType.encode}"
  }
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
