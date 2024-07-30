package compose.graphql

import zio.schema.meta.MetaSchema
import caliban.introspection.adt._
import scala.collection.mutable

final class TypeGenerator(graph: Graph) {
  self =>

  private val pending: mutable.Map[zio.schema.TypeId, Graph] = mutable.Map.empty

  private def notNull(t: __Type, cond: Boolean): __Type = if (cond) t.nonNull else t

  private def generateType(meta: zio.schema.meta.MetaSchema): __Type = {
    meta match {
      case MetaSchema.Product(id, _, fields, optional) =>
        val oldFields: List[__Field] = pending.get(id) match {
          case None        => Nil
          case Some(value) =>
            pending -= id
            value.cons.toList.map { case cons =>
              __Field(cons.name, None, Nil, () => generateType(cons.toType.ast))
            }
        }
        notNull(
          __Type(
            kind = __TypeKind.OBJECT,
            name = Some(id.name),
            fields = _ =>
              Some(
                fields.map(field => __Field(field._1, None, Nil, () => generateType(field._2)))
                  .toList ++ oldFields,
              ),
          ),
          !optional,
        )

      case MetaSchema.ListNode(item, _, optional) => notNull(
          __Type(kind = __TypeKind.LIST, name = None, ofType = Some(generateType(item))),
          !optional,
        )

      case MetaSchema.Value(valueType, _, optional) =>
        notNull(__Type(__TypeKind.SCALAR, name = Some(valueType.tag.capitalize)), !optional)

      // TODO: implement the rest of the cases
      // case MetaSchema.Ref(refPath, path, optional)             => ???
      // case MetaSchema.Dynamic(withSchema, path, optional)      => ???
      // case MetaSchema.Tuple(path, left, right, optional)       => ???
      // case MetaSchema.FailNode(message, path, optional)        => ???
      // case MetaSchema.Either(path, left, right, optional)      => ???
      // case MetaSchema.Dictionary(keys, values, path, optional) => ???
      // case MetaSchema.Sum(id, path, cases, optional)           => ???

      case schema => throw new MatchError(schema)
    }
  }

  def __type: __Type = {
    graph.cons.map(cons => cons.fromType.ast -> cons).collect {
      case (product: MetaSchema.Product, cons) => product.id -> cons
    }.foreach { case (id, cons) =>
      pending.get(id) match {
        case None        => pending += (id -> cons)
        case Some(value) => pending += id -> (value ++ cons)
      }
    }

    __Type(
      kind = __TypeKind.OBJECT,
      name = Some("Query"),
      fields = _ =>
        Some(
          graph.cons.filter(_.fromType == zio.schema.Schema[Unit])
            .map(cons => __Field(cons.name, None, Nil, () => generateType(cons.toType.ast))).toList,
        ),
    )
  }
}
