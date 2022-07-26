package compose

package object compose {
  type ~>[-A, +B] = Lambda[A, B]
}
