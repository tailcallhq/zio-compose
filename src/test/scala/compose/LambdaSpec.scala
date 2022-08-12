package compose

import zio.schema.{DeriveSchema, Schema}
import zio.schema.Schema._
import zio.test.{assertZIO, ZIOSpecDefault}
import zio.test.Assertion.equalTo

object LambdaSpec extends ZIOSpecDefault {
  case class FooBar(foo: Int, bar: Int)

  object FooBar {
    implicit val schema: Schema[FooBar] = DeriveSchema.gen[FooBar]
    val (foo, bar) = schema.makeAccessors(LambdaAccessor).asInstanceOf[(FooBar >>- Int, FooBar >>- Int)]
  }

  import Lambda._
  override def spec = suite("Lambda")(
    test("constant") {
      val res = constant(100)
      assertZIO(res.execute(()))(equalTo(100))
    },
    test("default") {
      val res = default[String]
      assertZIO(res.execute {})(equalTo(""))
    },
    test("addition") {
      val res = constant(1) + constant(2)
      assertZIO(res.execute {})(equalTo(3))
    },
    suite("fromMap")(
      test("key found") {
        val found = constant("A") >>> fromMap(Map[String, Int]("A" -> 100, "B" -> 200))
        assertZIO(found.execute {})(equalTo(Some(100)))
      },
      test("key not found") {
        val res = constant("C") >>> fromMap(Map[String, Int]("A" -> 100, "B" -> 200))
        assertZIO(res.execute {})(equalTo(None))
      },
    ),
    test("pipe") {
      val res = constant(1) >>> identity[Int]
      assertZIO(res.execute {})(equalTo(1))
    },
    test("zip") {
      val res = constant(1) <*> constant(2)
      assertZIO(res.execute {})(equalTo((1, 2)))
    },
    test("diverge") {
      val res = constant(true).diverge(isTrue = constant("Yes"), isFalse = constant("No"))
      assertZIO(res.execute {})(equalTo("Yes"))
    },
    suite("lens")(
      test("get") {
        val res = constant(FooBar(100, 200)) >>> FooBar.foo.get
        assertZIO(res.execute {})(equalTo(100))
      },
      test("set") {
        val res = constant(FooBar(100, 200)) <*> constant(1) >>> FooBar.foo.set
        assertZIO(res.execute {})(equalTo(FooBar(1, 200)))
      },
    ),
    test("transformation") {
      val res = constant(FooBar(1, 1)) >>> transform(
        FooBar.foo.get + constant(1) ->> FooBar.foo.set,
        FooBar.bar.get + constant(2) ->> FooBar.bar.set,
      )
      assertZIO(res.execute {})(equalTo(FooBar(2, 3)))
    },
  )
}
