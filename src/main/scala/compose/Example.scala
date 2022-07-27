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

  def program2 = ifElse(constant(true))(
    isTrue = constant("Yes"),
    isFalse = constant("No"),
  )

  def program3 = constant(10) > constant(2)

  def program4 =
    (default[User] zip (constant(Person("Tushar", "Mathur", 100)) >>> Person.age.get)) >>>
      User.age.set

  def program: Any ~> User =
    constant(Person("Tushar", "Mathur", 100)) >>>
      transform(
        Transform(Person.age.get, User.age.set),
        Transform(Person.lastName.get, User.name.set),
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

  case class Person(firstName: String, lastName: String, age: Int)
  object Person {
    val (firstName, lastName, age) = Schema[Person]
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(LambdaLens[Person, String], LambdaLens[Person, String], LambdaLens[Person, Int])]
  }

  case class User(name: String, age: Int)
  object User {
    val (name, age) = Schema[User]
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(LambdaLens[User, String], LambdaLens[User, Int])]
  }
}
