import compose.model.LambdaLens

package object compose {
  type ~>[-A, +B] = Lambda[A, B]
  type >>-[S, A]  = LambdaLens[S, A]
  type Remote[+A] = Lambda[Any, A]
}
