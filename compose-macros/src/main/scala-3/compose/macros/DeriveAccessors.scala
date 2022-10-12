package compose.macros

import zio.schema._

object DeriveAccessors {
  import scala.quoted.*
  import pprint.*

  trait DerivedLambdaAccessor[A] {
    type Lens
    def lens: Lens
  }

  inline def gen[A](using schema: Schema[A]): DerivedLambdaAccessor[A] = {
    ${ genImpl[A]('schema) }
  }

  private def genImpl[A: Type](
    schema: Expr[Schema[A]],
  )(using qctx: Quotes): Expr[DerivedLambdaAccessor[A]] = {
    import qctx.reflect.*

    def getType(a: Symbol) = a.tree match {
      case ValDef(_, tpt, _) => tpt.tpe.asType
      case _                 => throw new Error("No primary constructor found")
    }

    def accessor[L](using Type[L]) = '{
      new DerivedLambdaAccessor[A] {
        type Lens = L
        override def lens = ???

      }
    }

    val params = TypeRepr.of[A].typeSymbol.primaryConstructor.paramSymss.headOption.getOrElse(Nil)

    // val lens = '{type Foo[S, A] = _root_.compose.model.LambdaLens[S, A]} match {
    //   case '{ type Foo[$s, $a] = $l } => l
    // }

    // pprintln('{ type Foo = compose.macros.DeriveAccessors.DerivedLambdaAccessor[Int] }.asTerm)

    val expr = params.map(getType(_)) match {
      case List('[a])                   => accessor[a]
      case List('[a], '[b])             => accessor[(a, b)]
      case List('[a], '[b], '[c])       => accessor[(a, b, c)]
      case List('[a], '[b], '[c], '[d]) => accessor[(a, b, c, d)]

      case _ => throw new Error("No primary constructor found")
    }

    // pprintln(expr.show)

    expr
  }
}
