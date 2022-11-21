package compose.graphql

import compose.graphql.ast.Document

object NodePrinter {
  import compose.graphql.ast.TypeDefinition._
  def render(self: ast.Document): String = {
    self match {
      case Document(definitions) => s"\n${definitions.sortBy {
            case ObjectTypeDefinition(name, _) => name
            case FieldDefinition(name, _, _)   => name
            case InputValueDefinition(name, _) => name
          }.map(NodePrinter.render).mkString("\n")}\n"
    }
  }

  def render(self: ast.TypeDefinition): String = {
    self match {

      case ObjectTypeDefinition(name, fields) =>
        s"type $name {\n  ${fields.sortBy(_.name).map(NodePrinter.render(_)).mkString("\n  ")}\n}"

      case InputValueDefinition(name, fieldType) => s"$name: ${NodePrinter.render(fieldType)}"

      case FieldDefinition(name, arguments, fieldType) =>
        val args =
          if (arguments.isEmpty) ""
          else s"(${arguments.sortBy(_.name).map(NodePrinter.render(_)).mkString(", ")})"
        val tpe  = NodePrinter.render(fieldType)
        s"$name$args: $tpe"
    }
  }

  def render(fieldType: Type): String = {
    fieldType match {
      case NamedType(name)  => name
      case NotNullType(tpe) => s"${NodePrinter.render(tpe)}!"
      case ListType(tpe)    => s"[${NodePrinter.render(tpe)}]"
    }
  }
}
