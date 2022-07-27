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

  def isAllowed: Int ~> Boolean = ifElse(identity[Int] > constant(18))(
    isTrue = constant(true),
    isFalse = constant(false),
  )

  def program5 = constant(Person("Tushar", "Mathur", 50)) >>> transform(
    Transform(Person.age.get + constant(10), User.age.set),
    Transform(Person.firstName.get ++ constant(" ") ++ Person.lastName.get, User.name.set),
    Transform(Person.age.get >>> isAllowed, User.isAllowed.set),
  )

  def program = {
    constant(Fib(0, 1, 0)) >>>
      transform(
        Transform(Fib.b.get, Fib.a.set),
        Transform(Fib.a.get + Fib.b.get, Fib.b.set),
        Transform(Fib.i.get + constant(1), Fib.i.set),
      ).repeatUntil(Fib.i.get === constant(20)) >>> Fib.b.get
  }

  case class Fib(a: Int, b: Int, i: Int)
  object Fib {
    val (a, b, i) = Schema[Fib]
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(LambdaLens[Fib, Int], LambdaLens[Fib, Int], LambdaLens[Fib, Int])]
  }

  override def run =
    for {

      // Serialize the program to JSON
      json <- ZIO.succeed(program.compile.json)

      // _    <- ZIO.succeed(println(json))
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

  case class User(name: String, age: Int, isAllowed: Boolean)
  object User {
    val (name, age, isAllowed) = Schema[User]
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(LambdaLens[User, String], LambdaLens[User, Int], LambdaLens[User, Boolean])]
  }
}
