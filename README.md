# ZIO Compose

![workflow](https://github.com/tusharmath/graphql-compose/actions/workflows/ci.yml/badge.svg)

ZIO Compose is a library that helps you write programs that can be serialized and sent over the wire.

## Why should I serialize my program?

The basic idea is if code and data are on different machines, one of them must be moved to the other before the code can be executed on the data.
Typically, in big-data applications it's much more efficient to move code than the other way around.

There are other use-cases that don't involve big-data where you would want a serializable program. For eg: Instead a writing a json or yaml configuration to decide which control flow to execute, you encode the control flow itself, send it over the wire and execute it directly on the service.

ZIO Compose is a library that provides a type-safe way of writing programs that are serializable.

## Installation

Update your `build.sbt` by adding `zio-compose` as a dependency.

```scala
libraryDependencies += "io.tusharmath.github" % "zio-compose" %% version
```

## Getting started

1. Here is a simple program that sums two numbers -

   ```scala
   import compose.Lambda._

   val program = constant(1) + constant(2)
   ```

2. Running the ZIO compose program
   Programs can be executed using the default interpreter:

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

The core data type in ZIO Compose is `Lambda`. It is also type aliased by `~>` (tilde, greater than). A lambda `A ~> B` represents a serializable unary function that takes in an input of type `A` and produces and output of type `B`. For eg:

```scala
val c1: Any ~> Int = constant(100)
```

The above lambda `c1` is a function that takes in any input and produces an `Int` value.

Another example of lambda is `identity[A]` which like the scala's `identity` function, takes in a type `A` and returns the value as output. The only difference is that zio-compose's `identity` is serializable.

## Serialization

Any lambda from `A ~> B` can be serialized by performing a few steps.

```scala
val program = constant(1) + constant(2)
val compilation = program.compile //
```

## Advanced Example

Here is an advanced example of a program that calculates fibonacci numbers and is completely serializable.

```scala
  case class Fib(a: Int, b: Int, i: Int)
  object Fib {
    implicit val schema: Schema[Fib] = DeriveSchema.gen[Fib]
    val (a, b, i)                    = schema
      .makeAccessors(LambdaAccessor)
      .asInstanceOf[(Fib >>- Int, Fib >>- Int, Fib >>- Int)]
  }

  def fib = constant(Fib(0, 1, 0)) >>>
    transform(
      Fib.b.get             ->> Fib.a.set,
      Fib.a.get + Fib.b.get ->> Fib.b.set,
      Fib.i.get.inc         ->> Fib.i.set,
    ).repeatWhile(Fib.i.get =!= constant(20)) >>> Fib.b.get
```
