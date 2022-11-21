package compose.graphql.ast

import zio.Chunk

// scalafmt: { maxColumn = 180 }
sealed trait TypeDefinition
object TypeDefinition {
  final case class ObjectTypeDefinition(name: String, fields: Chunk[FieldDefinition])                     extends TypeDefinition
  final case class FieldDefinition(name: String, arguments: Chunk[InputValueDefinition], fieldType: Type) extends TypeDefinition
  final case class InputValueDefinition(name: String, fieldType: Type)                                    extends TypeDefinition

  sealed trait Type
  final case class NamedType(name: String) extends Type
  final case class NotNullType(tpe: Type)  extends Type
  final case class ListType(tpe: Type)     extends Type

  object syntax {}
}
