package compose.graphql

object AstPrinter {
  import Ast._
  def render(self: Ast): String = self match {
    case Ast.Document(definitions) =>
      s"\n${definitions.sortBy(_.name).map(AstPrinter.render(_)).mkString("\n")}\n"

    case definitions: Definition => definitions match {
        case Definition.ObjectTypeDefinition(name, fields) =>
          s"type $name {\n  ${fields.sortBy(_.name).map(AstPrinter.render(_)).mkString("\n  ")}\n}"

        case Definition.InputValueDefinition(name, fieldType) =>
          s"$name: ${AstPrinter.render(fieldType)}"

        case Definition.FieldDefinition(name, arguments, fieldType) =>
          val args =
            if (arguments.isEmpty) ""
            else s"(${arguments.sortBy(_.name).map(AstPrinter.render(_)).mkString(", ")})"
          val tpe  = AstPrinter.render(fieldType)
          s"$name$args: $tpe"
      }

    case fieldType: Ast.Type => fieldType match {
        case Type.NamedType(name)  => name
        case Type.NotNullType(tpe) => s"${AstPrinter.render(tpe)}!"
        case Type.ListType(tpe)    => s"[${AstPrinter.render(tpe)}]"
      }
  }
}
