package compose.macros

object DeriveAccessors {

  trait DerivedLambdaAccessor[A] {
    type Lens
    def lens: Lens
  }

  def gen[T](implicit schema: Schema[T]): DerivedLambdaAccessor[T] = new DerivedLambdaAccessor {
    type Lens = Unit
    def lens: Unit = ()
  }

}
