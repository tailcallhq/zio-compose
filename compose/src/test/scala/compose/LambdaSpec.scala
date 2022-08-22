package compose

import zio.durationInt
import zio.schema.{DeriveSchema, Schema}
import zio.schema.Schema._
import zio.test.{assertZIO, check, checkAll, Gen, ZIOSpecDefault}
import zio.test.Assertion.{equalTo, isTrue}
import zio.test.TestAspect.timeout

import scala.language.postfixOps
import compose.macros.DeriveAccessors

object LambdaSpec extends ZIOSpecDefault {

  import Lambda._

  override def spec = suite("Lambda")(
    test("constant") {
      val res = constant(100)
      assertZIO(res.eval(()))(equalTo(100))
    },
    test("default") {
      val res = default[String]
      assertZIO(res.eval {})(equalTo(""))
    },
    test("addition") {
      val res = constant(1) + constant(2)
      assertZIO(res.eval {})(equalTo(3))
    },
    suite("fromMap")(
      test("key found") {
        val found = constant("A") >>> fromMap(Map[String, Int]("A" -> 100, "B" -> 200))
        assertZIO(found.eval {})(equalTo(Some(100)))
      },
      test("key not found") {
        val res = constant("C") >>> fromMap(Map[String, Int]("A" -> 100, "B" -> 200))
        assertZIO(res.eval {})(equalTo(None))
      },
    ),
    test("pipe") {
      val res = constant(1) >>> identity[Int]
      assertZIO(res.eval {})(equalTo(1))
    },
    test("zip") {
      val res = constant(1) <*> constant(2)
      assertZIO(res.eval {})(equalTo((1, 2)))
    },
    test("diverge") {
      val res = constant(true).diverge(isTrue = constant("Yes"), isFalse = constant("No"))
      assertZIO(res.eval {})(equalTo("Yes"))
    },
    suite("lens")(
      test("get") {
        val res = constant(FooBar(100, 200)) >>> FooBar.accessors.foo.get
        assertZIO(res.eval {})(equalTo(100))
      },
      test("set") {
        val res = constant(FooBar(100, 200)) <*> constant(1) >>> FooBar.accessors.foo.set
        assertZIO(res.eval {})(equalTo(FooBar(1, 200)))
      },
    ),
    test("transformation") {
      val accessors = FooBar.accessors
      val res       = constant(FooBar(1, 1)) >>> transform(
        accessors.foo.get + constant(1) ->> accessors.foo.set,
        accessors.bar.get + constant(2) ->> accessors.bar.set,
      )
      assertZIO(res.eval {})(equalTo(FooBar(2, 3)))
    },
    test("bind") {
      val res = identity[Int].bind(100)
      assertZIO(res.eval {})(equalTo(100))
    },
    test("repeatWhile") {
      val res = constant(1) >>> (constant(2) * identity[Int]).repeatWhile {
        identity[Int] < constant(1024)
      }
      assertZIO(res.eval {})(equalTo(1024))
    },
    test("comparisons") {
      val gen = Gen.fromIterable(
        Seq(
          constant(1) < constant(2),
          constant(2) <= constant(2),
          constant(2) > constant(1),
          constant(2) >= constant(2),
        ),
      )

      checkAll(gen) { res => assertZIO(res.eval {})(isTrue) }
    },
    test("stats") {
      val res = constant(100) >>> stats(
        identity[Int] + constant(1),
        identity[Int] + constant(2),
        identity[Int] + constant(3),
      )
      assertZIO(res.eval {})(equalTo(103))
    },
    test("zip") {
      val res = constant(1) <*> constant(2)
      assertZIO(res.eval {})(equalTo((1, 2)))
    },
    test("zipRight") {
      val res = constant(1) *> constant(2)
      assertZIO(res.eval {})(equalTo(2))
    },
    test("zipLeft") {
      val res = constant(1) <* constant(2)
      assertZIO(res.eval {})(equalTo(1))
    },
    suite("scope")(
      test("get") {
        val res = scope { implicit ctx => Scope.make(1000).get }
        assertZIO(res.eval("OK!"))(equalTo(1000))
      },
      test("set") {
        val res = scope { implicit ctx =>
          val a = Scope.make(1000)

          (a := constant(1)) *> a.get
        }
        assertZIO(res.eval {})(equalTo(1))
      },
    ),
    suite("tuple")(
      test("_1") {
        val res = (constant(1) <*> constant(2))._1
        assertZIO(res.eval {})(equalTo(1))
      },
      test("_2") {
        val res = (constant(1) <*> constant(2))._2
        assertZIO(res.eval {})(equalTo(2))
      },
    ),
    test("doWhile") {
      val res = scope { implicit ctx =>
        val a = Scope.make(1)
        (a := a.get * constant(2)).doWhile(a.get < constant(1000)) *> a.get
      }
      assertZIO(res.eval {})(equalTo(1024))
    },
    suite("StringExecutionerations")(
      test("length") {
        val res = constant("ABC").length
        assertZIO(res.eval {})(equalTo(3))
      },
      test("upperCase") {
        val res = constant("abc").upperCase
        assertZIO(res.eval {})(equalTo("ABC"))
      },
      test("lowerCase") {
        val res = constant("ABC").lowerCase
        assertZIO(res.eval {})(equalTo("abc"))
      },
      test("startsWith") {
        val gen = Gen.fromIterable(Seq("A" -> true, "a" -> false))
        checkAll(gen) { case (str, expected) =>
          val res = constant("ABC").startsWith(constant(str))
          assertZIO(res.eval {})(equalTo(expected))
        }
      },
      test("endsWith") {
        val gen = Gen.fromIterable(Seq("C" -> true, "c" -> false))
        checkAll(gen) { case (str, expected) =>
          val res = constant("ABC").endsWith(constant(str))
          assertZIO(res.eval {})(equalTo(expected))
        }
      },
      test("contains") {
        val gen = Gen.fromIterable(Seq("A" -> true, "B" -> true, "c" -> false))
        checkAll(gen) { case (str, expected) =>
          val res = constant("ABC").contains(constant(str))
          assertZIO(res.eval {})(equalTo(expected))
        }
      },
    ),
    suite("logical")(
      test("and, or, not") {
        val boolean  = Gen.fromIterable(Seq(true, false))
        val boolean2 = boolean <*> boolean
        val andSeq   = boolean2.map { case (b1, b2) => (constant(b1) && constant(b2)) -> (b1 && b2) }
        val orSeq    = boolean2.map { case (b1, b2) => (constant(b1) || constant(b2)) -> (b1 || b2) }
        val notSeq   = boolean.map(b => constant(b).not -> !b)

        checkAll(andSeq ++ orSeq ++ notSeq) { case (lambda, expected) =>
          assertZIO(lambda.eval {})(equalTo(expected))
        }
      },
    ),
    suite("numeric")(
      test("integer comparison") {
        val int  = Gen.int
        val int2 = int <*> int

        val gt: Gen[Any, (Any ~> Boolean, Boolean)] = int2.map { case (b1, b2) =>
          (constant(b1) > constant(b2)) -> (b1 > b2)
        }

        val lt: Gen[Any, (Any ~> Boolean, Boolean)] = int2.map { case (b1, b2) =>
          (constant(b1) < constant(b2)) -> (b1 < b2)
        }

        val gte: Gen[Any, (Any ~> Boolean, Boolean)] = int2.map { case (b1, b2) =>
          (constant(b1) >= constant(b2)) -> (b1 >= b2)
        }

        val lte: Gen[Any, (Any ~> Boolean, Boolean)] = int2.map { case (b1, b2) =>
          (constant(b1) <= constant(b2)) -> (b1 <= b2)
        }

        check(gt ++ gte ++ lt ++ lte) { case (lambda, expected) =>
          assertZIO(lambda.eval {})(equalTo(expected))
        }
      },
      test("integer operation") {
        val int  = Gen.int
        val int2 = int <*> int

        val add    = int2.map { case (b1, b2) => (constant(b1) + constant(b2)) -> (b1 + b2) }
        val mul    = int2.map { case (b1, b2) => (constant(b1) * constant(b2)) -> (b1 * b2) }
        val div    = int2.map { case (b1, b2) => (constant(b1) / constant(b2)) -> (b1 / b2) }
        val sub    = int2.map { case (b1, b2) => (constant(b1) - constant(b2)) -> (b1 - b2) }
        val negate = int.map(b => constant(b).negate -> -b)

        check(add ++ mul ++ div ++ sub ++ negate) { case (lambda, expected) =>
          assertZIO(lambda.eval {})(equalTo(expected))
        }
      },
    ),
    test("toInt") {
      val seq = Gen.fromIterable(
        Seq[(Any ~> Either[String, Int], Either[String, Int])](
          constant("1").toInt   -> Right(1),
          constant("-1").toInt  -> Right(-1),
          constant("").toInt    -> Left("Cannot convert to Int"),
          constant("0.1").toInt -> Left("Cannot convert to Int"),
          constant("1.1").toInt -> Left("Cannot convert to Int"),
        ),
      )
      checkAll(seq) { case (isInt, expected) => assertZIO(isInt.eval {})(equalTo(expected)) }
    },
    suite("fold")(
      test("option") {
        val program = identity[Option[Int]].fold(constant(0), identity[Int].inc)
        val seq     = Gen.fromIterable(
          Seq(
            Option.empty[Int] -> 0,
            Option(1)         -> 2,
            Option(2)         -> 3,
          ),
        )

        checkAll(seq) { case (option, expected) =>
          assertZIO(program.eval(option))(equalTo(expected))
        }
      },
      test("either") {
        val program = identity[Either[Int, Int]].fold(identity[Int].dec, identity[Int].inc)
        val seq     = Gen.fromIterable(
          Seq(
            Left(1)  -> 0,
            Right(1) -> 2,
          ),
        )

        checkAll(seq) { case (option, expected) =>
          assertZIO(program.eval(option))(equalTo(expected))
        }
      },
    ),
    suite("optional")(
      test("isEmpty") {
        check(Gen.option(Gen.int)) { optional =>
          val res = constant(optional).isEmpty
          assertZIO(res.eval {})(equalTo(optional.isEmpty))
        }
      },
      test("isNonEmpty") {
        check(Gen.option(Gen.int)) { optional =>
          val res = constant(optional).nonEmpty
          assertZIO(res.eval {})(equalTo(optional.nonEmpty))
        }
      },
    ),
  ) @@ timeout(5 second)

  case class FooBar(foo: Int, bar: Int)

  object FooBar {
    implicit val schema: Schema[FooBar] = DeriveSchema.gen[FooBar]
    val accessors                       = DeriveAccessors.gen[FooBar]
  }
}
