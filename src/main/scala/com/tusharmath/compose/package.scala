package com.tusharmath

package object compose {
  type ~>[-A, +B] = Lambda[A, B]
}
