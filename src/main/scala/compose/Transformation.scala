package compose

import zio.schema.Schema

sealed trait Transformation[-A, +B] { self => }

object Transformation {
  case class Constructor[A, B, I](f: A ~> I, i: Schema[I], g: (B, I) ~> B) extends Transformation[A, B] {
    def apply(f1: A ~> B)(implicit b: Schema[B]): A ~> B = f1.zip(f)(b, i) >>> (g: (B, I) ~> B)
  }

  def apply[A, B, I](f: A ~> I, g: (B, I) ~> B)(implicit i: Schema[I]): Transformation[A, B] =
    Constructor(f, i, g)
}
