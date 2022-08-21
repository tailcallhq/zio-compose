package examples

import zio.{ZIO, ZIOAppDefault}
import zio.schema.{DeriveSchema, Schema}
import compose.macros.DeriveAccessors
import compose._
import compose.interpreter.Interpreter

object Example extends ZIOAppDefault {
  import compose.Lambda._

  // WAP to sum two numbers
  def program1: Any ~> Int = constant(1) + constant(2)

  // conditionally execute one of the two programs
  def program2 = constant(true).diverge(
    isTrue = constant("Yes"),
    isFalse = constant("No"),
  )

  // numeric comparison
  def program3 = constant(10) > constant(2)

  // lense for accessing the value of a user
  def program4 =
    (default[User] zip (constant(Person("Tushar", "Mathur", 100)) >>> Person.lens.age.get)) >>>
      User.lens.age.set

  // transforming a person into a user
  def program5 = constant(Person("Tushar", "Mathur", 50)) >>> transform(
    (Person.lens.age.get + constant(10))                                     ->> User.lens.age.set,
    (Person.lens.firstName.get ++ constant(" ") ++ Person.lens.lastName.get) ->> User.lens.name.set,
    (Person.lens.age.get > constant(18))                                     ->> User.lens.isAllowed.set,
  )

  // calculating fib(20)
  def program6 = {
    constant(Fib(0, 1, 0)) >>>
      transform(
        Fib.lens.b.get                  ->> Fib.lens.a.set,
        Fib.lens.a.get + Fib.lens.b.get ->> Fib.lens.b.set,
        Fib.lens.i.get.inc              ->> Fib.lens.i.set,
      ).repeatWhile(Fib.lens.i.get =!= constant(20)) >>> Fib.lens.b.get
  }

  // testing if the sum of three numbers is greater than their product
  def program7 = (constant(2) <*> constant(2) <*> constant(3)) >>> scope { implicit ctx =>
    val a      = Scope.make(0)
    val b      = Scope.make(0)
    val c      = Scope.make(0)
    val result = Scope.make(false)
    val input  = identity[((Int, Int), Int)]

    stats(
      a      := input._1._1,
      b      := input._1._2,
      c      := input._2,
      result := a.get + b.get > a.get * b.get,
    ) *> result.get
  }

  // fibonacci using mutables scopes
  def program8 = constant(10) >>> scope { implicit ctx =>
    val a = Scope.make(0)
    val b = Scope.make(1)
    val n = Scope.make(0)
    val i = Scope.make(1)

    stats(
      n := a.get + b.get,
      a := b.get,
      b := n.get,
      i := i.get.inc,
    ).doWhile { i.get < identity[Int] } *> n.get
  }

  def guessANumber: Any ~> Unit =
    writeLine("Guess a number between 1 and 10")

  override def run =
    for {
      int <- Interpreter.eval(program6)
      _   <- ZIO.succeed(println(int))
    } yield ()

  case class Fib(a: Int, b: Int, i: Int)

  case class Person(firstName: String, lastName: String, age: Int)

  case class User(name: String, age: Int, isAllowed: Boolean)

  object Fib {
    implicit val schema: Schema[Fib] = DeriveSchema.gen[Fib]
    val lens                         = DeriveAccessors.gen[Fib]
  }

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
    val lens                            = DeriveAccessors.gen[Person]
  }

  object User {
    implicit val schema: Schema[User] = DeriveSchema.gen[User]
    val lens                          = DeriveAccessors.gen[User]
  }
}
