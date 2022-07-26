package compose

import zio.{ZIO, ZIOAppDefault}
import zio.schema.codec.JsonCodec
import zio.schema.{DynamicValue, Schema}
import zio.schema.DeriveSchema.gen

object Example extends ZIOAppDefault {

  import Lambda._
  import IsNumeric._

  // WAP to sum two numbers
  def program1: Any ~> Int = constant(1) + constant(2)

  def program = ifElse(constant(true))(
    isTrue = constant("Yes"),
    isFalse = constant("No"),
  )

  override def run =
    for {

      // Serialize the program to JSON
      json <- ZIO.succeed(program.compile.json)

      // Deserialize the program from JSON
      plan <- ExecutionPlan.fromJson(json)

      // Execute the program
      unit = Schema.primitive[Unit].toDynamic(())
      res <- Interpreter.eval(plan, unit)

      // Serialize and print the output
      resJson <- ZIO.succeed(
        new String(JsonCodec.encode(Schema[DynamicValue])(res).toArray),
      )
      _       <- ZIO.succeed(println(resJson))
    } yield ()

  case class User(name: String, age: Int)
  object User {
    val (name, age) = Schema[User]
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(User ~> String, User ~> Int)]
  }
}
