# ZIO Compose

![workflow](https://github.com/tusharmath/graphql-compose/actions/workflows/ci.yml/badge.svg)

ZIO Compose is a library that helps you write programs that can be serialized and sent over the wire.

## Introduction

The basic idea behind having serializable programs is if code and data are on different machines, one of them **must be
moved** to the other before the code can be executed on the data.
Typically, in big-data applications it's much more efficient to move code than the other way around.

There are other use-cases that don't involve big-data where you would want a serializable program. For eg: Instead of
reading a json or yaml configuration in your application, to decide which control flow to execute, you encode the
control flow itself, send it over the wire to your application and execute that directly.

ZIO Compose intends to take care of such use cases. It intends to provide a complete DSL to write any kind of
distributed computation using Scala in a type-safe manner. It's built on top of [ZIO Schema].

[zio schema]: https://github.com/zio/zio-schema/pulls

## Installation

Update your `build.sbt` by adding `zio-compose` as a dependency.

```scala
libraryDependencies += "com.tusharmath" % "zio-compose" %% version
```

# Getting started

1. Here is a simple program that adds two numbers -

   ```scala
   import compose.Lambda._

   val program = constant(1) + constant(2)
   ```

2. Programs can be executed using the default interpreter:

   ```scala
   import zio._

   object ZIOCompose extends ZIOAppDefault {
     val run = for {
       res <- Interpreter.eval(program)
       _   <- ZIO.succeed(println(s"Result: ${res}"))
     } yield ()
   }
   ```

## Lambda

The core data type in ZIO Compose is `Lambda`. It is also type aliased by `~>` (tilde, greater than). A lambda `A ~> B`
represents a serializable unary function that takes in an input of type `A` and produces and output of type `B`. For eg:

```scala
val c1: Any ~> Int = Lambda.constant(100)
```

The above lambda `c1` is a function that takes in any input and produces an `Int` value.

Another example of lambda is `identity[A]` which like the scala's `identity` function, takes in a type `A` and returns
itself as output. The only difference is that zio-compose's `identity` is serializable.

## Serialization

Any lambda from `A ~> B` can be serialized into JSON by performing a few steps.

```scala
// A program that adds two numbers
val program: Any ~> Int = constant(1) + constant(2)

// Call the `compile` method to create an execution plan
val compilation: ExecutionPlan = program.compile

// call `json` on the execution plan to encode it as JSON
val json: String = compilation.compile.json
```

## Conditional Operations

Conditional operations can be implemented on `Lambda`s that return a `Boolean` using the `diverge` operator.
The following program returns `"Yes"` if the condition is true and `"No"` if the condition is false.

```scala
import Lambda._

val program = (constant(1) > constant(2)).diverge(
  isTrue = constant("Yes"),
  isFalse = constant("No")
)
```

Since `1 < 2` the condition is `false` and the output thus becomes `"no"`.

## Piping Lambdas

Two lambdas can be composed using the `pipe` or `compose` operator. For eg: if there exists a lambda `l1: A ~> B` and a
lambda `l2: B ~> C` then they can be composed using the pipe operator as —

```scala
val l1: A ~> B  = ???
val l2: B ~> C  = ???
val l12: A ~> C = A >>> B
```

This is the semantic equivalent of `l2(l1(a))` , where `a` is of type `A`.

## Lenses

ZIO Compose has support for lenses which allows very precise control over getting and setting values over record types.
For eg: Let's say there is a type `User` and we want to get the `age` of that user. We could do something like this —

```scala
import zio.schema._
import compose.macros.DeriveAccessors

case class User(firstName: String, lastName: String, age: Int)
object User {

  // Derive the Schema
  implicit val schema: Schema[User] = DeriveSchema.gen[User]

  // Derive accessors
  val lens = DeriveAccessors.gen[User]
}
```

The `schema` field inside of `User` provides access to the meta-data and structure of the type `User`.
Whereas `lens` internally uses `schema` to navigate through an instance to lookup or update it's fields in a type-safe manner. Let's see that in action —

```scala
val user: Any ~> User = constant(User("John", "Doe", 23))
val age: User ~> Int  = User.lens.age.get
val program: Any ~> Int = user >>> age
```

Here we create a user using `constant` and then using the derived lens we create a Lambda from `User ~> Int`.
We compose the two lambdas together using the `>>>` operator (alias to `pipe`).
The final program is a type-safe, serializable function that can take anything and produce an integer.

Now let's look at an example where we are updating a field using lenses in the User type -

```scala
val user: Any ~> User = constant(User("John", "Doe", 23))
val program: Any ~> User = (user <*> constant(12)) >>> User.lens.age.set
```

The `set` methods on lens is a binary function, so it needs two arguments - 1. The whole object which needs to be updated and 2. the value it needs to set. In our case `age.set` would have a type like this - `(User, Int) ~> User`. That's why we use the `<*>` operator (alias to `zip`) to combine the two inputs and send it to the `set` function.

## Transformations

Transformations from one type to another are easily possible using the lens API, however it can become a bit verbose and boilerplate sometimes.
ZIO Compose provides a DSL to simplify transformations. Here is an example of converting `User` to `Customer`, we start by defining the types, schema and it's lens.

```scala
case class Customer(name: String, age: Int, allowed: Boolean)
object Customer {
  implicit val schema = DeriveSchema.gen[Customer]
  val lens = DeriveAccessors.gen[Customer]
}
```

We then take each field of the user, perform some transformations on the field themselves and then set it in a customer. A transformation can be defined using the `->>` operator.

```scala
val t1: User ~> Customer = (User.lens.age.get + constant(10)) ->> Customer.lens.age.set,
```

A `Transformation`, is nothing but a pair of a getter and setter. We can combine multiple transformations using the `transform` operator -

```scala
import Lambda._

val user2Customer: User ~> Customer = transform(
  (User.lens.age.get + constant(10))                                     ->> Customer.lens.age.set,
  (User.lens.firstName.get ++ constant(" ") ++ Person.lens.lastName.get) ->> Customer.lens.name.set,
  (User.lens.age.get > constant(18))                                     ->> Customer.lens.isAllowed.set,
)
```

The final output of the transformation is a function from `User ~> Customer`. We can then pipe in an actual user instance to produce a customer as follows —

```scala
val program: Any ~> Customer = constant(User("John", "Doe", 20)) >>> user2Customer
```

## Looping

With ZIO Compose one can loop over a lambda in multiple ways. For eg:
Let's say I want to add all numbers between 0 to 10. We can do this by creating a type `Sum` which maintains intermediary state of our program like this —

```scala
import compose.macros.DeriveSchema

case class Sum(count: Int, result: Int)
object Sum {
  implicit val schema = DeriveSchema.gen[Sum]
  val lens = DeriveAccessor.gen[Sum]
}
```

Then we make a lambda of type `Sum ~> Sum` to represent one iteration of our loop. In the iteration we perform two operations -

1. Increase the value of `count` by one.
2. Add the value of `count` to `result`.

```scala
import Lambda._

val iteration: Sum ~> Sum = transform(
  Sum.lens.count.get.inc                   ->> Sum.lens.count.set,
  Sum.lens.result.get + Sum.lens.count.get ->> Sum.lens.result.set
)
```

We then use the `repeatWhile` operator to keep iterating while the condition is true.

```scala
val sum: Any ~> Sum = iteration.repeatWhile(Sum.lens.count.get < constant(10))
```

To get the exact value of the sum we can again use the lens API as follows —

```scala
val program: Any ~> Int = sum >>> Sum.lens.result.get
```

## Scopes

Scopes allows us to define and update variables within a given context.
They turn out to be pretty handy when we want to share some data across different part of our program without having to pass it using `pipe`.
Below we take an arbitrary example where have two numbers and we want to check if their sum is greater than their product.

```scala
import Lambda._
val program = scope { implicit ctx =>
  val a = Scope(10)
  val b = Scope(5)
  val result = Scope(false)

  (a.get + b.get) > (a.get * b.get) >>> result.set
}
```

A `Scope` is like a `ZRef` with `get` and `set` methods on it. It can be initialized with a default value.
However, it can only be initialized inside a `scope { }` block. The `{implicit ctx =>` provides context in which the scoped variable is available.

## Advanced Example

Here is an advanced example of a program that calculates fibonacci numbers and is completely serializable.

```scala
import compose._
import zio.schema._

case class Fib(a: Int, b: Int, i: Int)
object Fib {
  implicit val schema: Schema[Fib] = DeriveSchema.gen[Fib]
  val lens = DeriveAccessor.gen[Fib]
}

def fib = constant(Fib(0, 1, 0)) >>>
  transform(
    Fib.lens.b.get                  ->> Fib.lens.a.set,
    Fib.lens.a.get + Fib.lens.b.get ->> Fib.lens.b.set,
    Fib.lens.i.get.inc              ->> Fib.lens.i.set,
  ).repeatWhile(Fib.lens.i.get =!= constant(20)) >>> Fib.lens.b.get
```
