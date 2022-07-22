package com.tusharmath.compose.internal

trait EitherExtensions {
  implicit class EitherZip[E, A](self: Either[E, A]) {
    def zip[A1](other: Either[E, A1]): Either[E, (A, A1)] =
      self match {
        case Left(value)  => Left(value)
        case Right(value) =>
          other match {
            case Left(value1)  => Left(value1)
            case Right(value1) => Right((value, value1))
          }
      }
  }
}
