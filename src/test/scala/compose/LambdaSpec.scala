package compose

import zio.schema.Schema._
import zio.test.{assertZIO, ZIOSpecDefault}
import zio.test.Assertion.equalTo

object LambdaSpec extends ZIOSpecDefault {
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
  )
}
