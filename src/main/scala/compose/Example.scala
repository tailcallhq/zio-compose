package compose

import zio.{ZIO, ZIOAppDefault}
import zio.schema.codec.JsonCodec
import zio.schema.{DynamicValue, Schema}
import zio.schema.DeriveSchema

object Example extends ZIOAppDefault {

  import Lambda._
  import IsNumeric._
  import Person._

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
    Transformation(Person.age.get + constant(10), User.age.set),
    Transformation(Person.firstName.get ++ constant(" ") ++ Person.lastName.get, User.name.set),
    Transformation(Person.age.get >>> isAllowed, User.isAllowed.set),
  )

  def program6 = {
    constant(Fib(0, 1, 0)) >>>
      transform(
        Fib.b.get             ->> Fib.a.set,
        Fib.a.get + Fib.b.get ->> Fib.b.set,
        Fib.i.get.inc         ->> Fib.i.set,
      ).repeatUntil(Fib.i.get === constant(20)) >>> Fib.b.get
  }

  def program7 = (constant(2) <*> constant(2) <*> constant(3)) >>> scope { implicit ctx =>
    val a      = Scope(0)
    val b      = Scope(0)
    val c      = Scope(0)
    val result = Scope(false)
    val input  = identity[((Int, Int), Int)]

    seq(
      a      := input >>> arg0 >>> arg0,
      b      := input >>> arg0 >>> arg1,
      c      := input >>> arg1,
      result := a.get + b.get > a.get * b.get,
    ) *> result.get
  }

  def program = constant(20) >>> scope { implicit ctx =>
    val a = Scope(0)
    val b = Scope(1)
    val n = Scope(0)
    val i = Scope(1)

    seq(
      n := a.get + b.get,
      a := b.get,
      b := n.get,
      i := i.get.debug("i") + constant(1),
    ).repeatUntil((i.get === identity[Int]).debug("Condition")) *> n.get
  }

  case class Fib(a: Int, b: Int, i: Int)
  object Fib {
    implicit val schema: Schema[Fib] = DeriveSchema.gen[Fib]
    val (a, b, i)                    = schema
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(Fib >>- Int, Fib >>- Int, Fib >>- Int)]
  }

  override def run =
    for {
      res <- Interpreter.evalDynamic(program)

      // Serialize and print the output
      resJson <- ZIO.succeed(
        new String(JsonCodec.encode(Schema[DynamicValue])(res).toArray),
      )
      _       <- ZIO.succeed(println(resJson))
    } yield ()

  case class Person(firstName: String, lastName: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
    val (firstName, lastName, age)      = schema
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(Person >>- String, Person >>- String, Person >>- Int)]
  }

  case class User(name: String, age: Int, isAllowed: Boolean)
  object User {
    implicit val schema: Schema[User] = DeriveSchema.gen[User]

    val (name, age, isAllowed) = schema
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(User >>- String, User >>- Int, User >>- Boolean)]
  }
}
