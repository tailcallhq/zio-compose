package compose.graphql.ast

object AstPrinter {
  import Ast._
  def render(self: Ast): String = self match {
    case Ast.Document(definitions) =>
      s"\n${definitions.sortBy { case Definitions.ObjectType(name, _) => name }.map(_.encode).mkString("\n")}\n"

    case definitions: Ast.Definitions => definitions match {
        case Definitions.ObjectType(name, fields) =>
          s"type $name {\n${fields.map(_.encode).mkString("\n")}\n}"
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
