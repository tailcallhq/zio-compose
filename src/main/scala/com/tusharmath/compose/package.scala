package com.tusharmath

import com.tusharmath.compose.internal.EitherExtensions

package object compose extends EitherExtensions {
  type ~>[A, B] = Lambda[A, B]
}
