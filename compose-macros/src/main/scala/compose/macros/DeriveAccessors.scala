package compose.macros

object DeriveAccessors {

  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox
  import zio.schema.Schema

  trait DerivedLambdaAccessor[A] {
    type Lens
    def lens: Lens
  }

  def gen[T](implicit schema: Schema[T]): DerivedLambdaAccessor[T] =
    macro genImpl[T]

  def genImpl[T](c: whitebox.Context)(schema: c.Expr[Schema[T]])(implicit ev: c.WeakTypeTag[T]): c.Tree = {

    import c.universe._

    val tpe = weakTypeOf[T]

    println()

    val params = tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.headOption
    val quotes = params match {
      case None         => throw new Error("No primary constructor found")
      case Some(params) =>
        q"""        
        import _root_.compose.macros.DeriveAccessors.DerivedLambdaAccessor
                
        new DerivedLambdaAccessor[${weakTypeOf[T]}] {
          type Lens = ${tq"(..${params.map(i => tq"_root_.compose.model.LambdaLens[${tpe}, ${i.typeSignature}]")})"}
          override def lens: Lens = ${schema}.makeAccessors(_root_.compose.internal.LambdaAccessor).asInstanceOf[Lens]
          val ${pq"(..${params.map(i => i.name.encodedName)})"} = this.lens
        }
        """
    }

    quotes
  }

}
