package compose.graphql.internal

import compose.graphql.ast.OperationDefinition
import compose.graphql.ast.OperationDefinition.Argument
import zio.schema.{DynamicValue, StandardType, TypeId}

import scala.collection.immutable.ListMap

object DynamicValueUtility {
  def fromValue(value: OperationDefinition.Value): DynamicValue = {
    value match {
      case OperationDefinition.IntValue(value)     => DynamicValue
          .Primitive(value, StandardType.IntType)
      case OperationDefinition.StringValue(value)  => DynamicValue
          .Primitive(value, StandardType.StringType)
      case OperationDefinition.BooleanValue(value) => DynamicValue
          .Primitive(value, StandardType.BoolType)
      case OperationDefinition.VariableValue(_)    => ???
    }
  }

  def fromArguments(argument: Seq[Argument]): DynamicValue = {
    DynamicValue.Record(
      TypeId.Structural,
      ListMap.from(argument.map { arg => arg.name -> fromValue(arg.value) }),
    )
  }
}
