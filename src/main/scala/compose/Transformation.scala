package compose

import zio.schema.Schema

sealed trait Transformation[-A, +B]
object Transformation {
  case class Constructor[A, B, I](f: A ~> I, i: Schema[I], g: (B, I) ~> B) extends Transformation[A, B]
  def apply[A, B, I](f: A ~> I, g: (B, I) ~> B)(implicit i: Schema[I]): Transformation[A, B] = Constructor(f, i, g)
}
