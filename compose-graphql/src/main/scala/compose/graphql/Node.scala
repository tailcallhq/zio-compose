package compose.graphql

import zio.Chunk

/**
 * GraphQL is the AST used to represent a GraphQL query,
 * mutation, subscription or schema. The AST can be
 * generated in two ways viz. from a string. and from a list
 * of connections.
 */
sealed trait Node

object Node {
  // scalafmt: { maxColumn = 180 }

  final case class Document(definitions: Chunk[Definition]) extends Node

  sealed trait Definition                                                                                 extends Node
  final case class ObjectTypeDefinition(name: String, fields: Chunk[FieldDefinition])                     extends Definition
  final case class FieldDefinition(name: String, arguments: Chunk[InputValueDefinition], fieldType: Type) extends Definition
  final case class InputValueDefinition(name: String, fieldType: Type)                                    extends Definition
  final case class OperationDefinition(operation: Operation, name: Option[String], selectionSet: Chunk[Field])

  final case class Field(name: String, selection: Chunk[Field] = Chunk.empty)

  sealed trait Type                        extends Node
  final case class NamedType(name: String) extends Type
  final case class NotNullType(tpe: Type)  extends Type
  final case class ListType(tpe: Type)     extends Type

  sealed trait Operation
  case object QueryOperation        extends Operation
  case object MutationOperation     extends Operation
  case object SubscriptionOperation extends Operation
}
