package compose.graphql.ast

import compose.graphql.Graph
import compose.graphql.internal.SchemaExtensions.Extensions
import compose.graphql.ast.TypeDefinition._
import zio.Chunk
import zio.schema.TypeId.{Nominal, Structural}
import zio.schema.{Schema, StandardType, TypeId}

import scala.collection.mutable

final case class Document(definitions: Chunk[TypeDefinition]) {
  def render: String = Document.Printer.render(this)
}
object Document                                               {

  object Factory {

    def getArguments(schema: Schema[_]): Chunk[InputValueDefinition] = schema match {
      case schema: Schema.Record[_] => schema.fields.map { field =>
          InputValueDefinition(field.name, getFieldType(field.schema))
        }

      case schema @ Schema.Primitive(standardType, _) if standardType != StandardType.UnitType =>
        Chunk(InputValueDefinition("value", getFieldType(schema)))

      case _ => Chunk.empty
    }

    def getFields(schema: Schema.Record[_]): Chunk[FieldDefinition] = schema.fields
      .map(field => FieldDefinition(field.name, Chunk.empty, getFieldType(field.schema)))

    def getObjectType(schema: Schema[_]): Seq[ObjectTypeDefinition] = {
      schema.eval match {
        case Schema.Optional(schema, _) => getObjectType(schema)

        case Schema.Sequence(schemaA, _, _, _, _) => getObjectType(schemaA)

        case schema: Schema.Record[_] =>
          val children = schema.fields.map(_.schema).filter(_.isRecord).flatMap(getObjectType)
          val fields   = Seq(ObjectTypeDefinition(getName(schema.id), getFields(schema)))
          children ++ fields

        case Schema.Primitive(_, _) => Chunk(ObjectTypeDefinition("Query", Chunk.empty))

        // Unhandled
        //      case Schema.Tuple(left, right, annotations)                => ???
        //      case Schema.Fail(message, annotations)                     => ???
        //      case Schema.MapSchema(ks, vs, annotations)                 => ???
        //      case Schema.SetSchema(as, annotations)                     => ???
        //      case schema: Schema.Enum[_]                                => ???
        //      case Schema.Dynamic(annotations)                           => ???
        //      case Schema.Transform(schema, f, g, annotations, identity) => ???
        //      case Schema.Meta(ast, annotations)                         => ???
        //      case Schema.Lazy(schema0)                                  => ???
        //      case Schema.SemiDynamic(defaultValue, annotations)         => ???
        //      case Schema.EitherSchema(left, right, annotations)         => ???
        case schema => throw new MatchError(schema)
      }
    }

    def getName(typeId: TypeId): String = {
      typeId match {
        case Nominal(_, _, typeName) => typeName
        case Structural              => "Structural"
      }
    }

    def getName(schema: Schema[_]): String = {
      schema match {
        case schema: Schema.Record[_]                   => getName(schema.id)
        case Schema.Primitive(StandardType.UnitType, _) => "Query"
        case _ => throw new NotImplementedError(s"${schema.getClass.getName}")
      }
    }

    def getFieldType(schema: Schema[_]): Type = {
      def loop(schema: Schema[_], isRequired: Boolean): Type = {
        schema.eval match {
          case Schema.Optional(schema, _) => loop(schema, false)

          case Schema.Sequence(schemaA, _, _, _, _) =>
            val fieldType = ListType(getFieldType(schemaA))
            if (isRequired) NotNullType(fieldType) else fieldType

          case schema: Schema.Record[_] =>
            val fieldType = NamedType(getName(schema.id))
            if (isRequired) NotNullType(fieldType) else fieldType

          case Schema.Primitive(standardType, _) =>
            val fieldType = NamedType(standardType.tag.capitalize)

            if (isRequired) NotNullType(fieldType) else fieldType

          // Unhandled
          //      case Schema.Tuple(_, _, _)                => ???
          //      case Schema.Fail(message, annotations)                     => ???
          //      case Schema.MapSchema(ks, vs, annotations)                 => ???
          //      case Schema.SetSchema(as, annotations)                     => ???
          //      case schema: Schema.Enum[_]                                => ???
          //      case Schema.Dynamic(annotations)                           => ???
          //      case Schema.Transform(schema, _, _, _, _)                  => ???
          //      case Schema.Meta(ast, annotations)                         => ???
          //      case Schema.SemiDynamic(defaultValue, annotations)         => ???
          //      case Schema.EitherSchema(left, right, annotations)         => ???
          case schema                            => throw new MatchError(schema)
        }
      }

      loop(schema, true)

    }

    def getTypeDefinitions(connections: Chunk[Graph.Cons]): Chunk[ObjectTypeDefinition] = {
      val definitions = mutable.Set.empty[ObjectTypeDefinition]

      connections.foreach { case Graph.Cons(name, arg, from, to, _) =>
        val fromName      = getObjectType(from)
        val toName        = getObjectType(to)
        val conField      = FieldDefinition(name, getArguments(arg), getFieldType(to))
        val conObjectType = ObjectTypeDefinition(getName(from), Chunk(conField))
        definitions.addAll(fromName).addAll(toName).add(conObjectType)
      }

      mergeTypeDefinitions(definitions.toSeq)
    }

    def mergeTypeDefinitions(
      typeDefinitions: Seq[ObjectTypeDefinition],
    ): Chunk[ObjectTypeDefinition] = {
      val merged = mutable.Map.empty[String, ObjectTypeDefinition]

      typeDefinitions.foreach { definition =>
        merged.get(definition.name) match {
          case Some(ObjectTypeDefinition(name, fields)) => merged
              .put(name, ObjectTypeDefinition(name, fields ++ definition.fields))
          case None                                     => merged.put(definition.name, definition)
        }
      }

      Chunk.fromIterable(merged.values.toSeq)
    }
  }

  object Printer {
    def render(self: Document): String = {
      self match {
        case Document(definitions) => s"\n${definitions.sortBy {
              case ObjectTypeDefinition(name, _) => name
              case FieldDefinition(name, _, _)   => name
              case InputValueDefinition(name, _) => name
            }.map(Printer.render).mkString("\n")}\n"
      }
    }

    def render(self: TypeDefinition): String = {
      self match {

        case ObjectTypeDefinition(name, fields) =>
          s"type $name {\n  ${fields.sortBy(_.name).map(Printer.render(_)).mkString("\n  ")}\n}"

        case InputValueDefinition(name, fieldType) => s"$name: ${Printer.render(fieldType)}"

        case FieldDefinition(name, arguments, fieldType) =>
          val args =
            if (arguments.isEmpty) ""
            else s"(${arguments.sortBy(_.name).map(Printer.render(_)).mkString(", ")})"
          val tpe  = Printer.render(fieldType)
          s"$name$args: $tpe"
      }
    }

    def render(fieldType: Type): String = {
      fieldType match {
        case NamedType(name)  => name
        case NotNullType(tpe) => s"${Printer.render(tpe)}!"
        case ListType(tpe)    => s"[${Printer.render(tpe)}]"
      }
    }
  }

  def fromGraph(graph: Graph): Document =
    Document(Factory.getTypeDefinitions(Chunk.fromIterable(graph.cons)))
}
