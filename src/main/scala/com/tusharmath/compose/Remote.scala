package com.tusharmath.compose

import zio.schema.Schema

sealed trait Remote[A] { self =>
  def &&(other: Remote[Boolean])(implicit
    ev: A =:= Boolean,
  ): Remote[Boolean] =
    Remote.And(self.asBoolean, other)

  def >(other: Remote[Int])(implicit ev: A =:= Int): Remote[Boolean] =
    self gt other

  def <(other: Remote[Int])(implicit ev: A =:= Int): Remote[Boolean] =
    other gt self.asInt

  def +(other: Remote[Int])(implicit ev: A =:= Int): Remote[Int] = add(other)

  def add(other: Remote[Int])(implicit ev: A =:= Int): Remote[Int] =
    Remote.add(self.asInt, other)

  def asBoolean(implicit ev: A =:= Boolean): Remote[Boolean] =
    self.asInstanceOf[Remote[Boolean]]

  def asInt(implicit ev: A =:= Int): Remote[Int] =
    self.asInstanceOf[Remote[Int]]

  def executable: Executable = Remote.compile(self)

  def gt(other: Remote[Int])(implicit ev: A =:= Int): Remote[Boolean] =
    Remote.GreaterThan(self.asInstanceOf[Remote[Int]], other)

  def map[B](f: Remote[A => B]): Remote[B] = Remote.RemoteMap(self, f)
}

object Remote {

  def add(first: Remote[Int], second: Remote[Int]): Remote[Int] =
    AddInteger(first, second)

  def between(
    min: Remote[Int],
    max: Remote[Int],
    input: Remote[Int],
  ): Remote[Boolean] =
    (input > min) && (input < max)

  def ifElse[A](
    cond: Remote[Boolean],
  )(ifTrue: Remote[A], ifFalse: Remote[A]): Remote[A] =
    IfElse(cond, ifTrue, ifFalse)

  def length: Remote[String => Int] = Length

  def lit[A](a: A)(implicit s: Schema[A]): Remote[A] =
    Literal(a, s)

  def upperCase: Remote[String => String] = UpperCase

  private[compose] def compile[A](self: Remote[A]): Executable =
    self match {
      case And(left, right) => Executable.And(left.executable, right.executable)
      case Literal(a, s)    => Executable.Literal(s.toDynamic(a))
      case AddInteger(first, second) =>
        Executable.AddInteger(first.executable, second.executable)

      case IfElse(cond, isTrue, isFalse) =>
        Executable.IfElse(cond.executable, isTrue.executable, isFalse.executable)

      case GreaterThan(first, second) =>
        Executable.GreaterThan(first.executable, second.executable)

      case RemoteMap(r, f) =>
        Executable.ExecMap(r.executable, f.executable)

      case UpperCase => Executable.UpperCase

      case Length => Executable.Length
    }

  final case class RemoteMap[A, B](remote: Remote[A], f: Remote[A => B])
      extends Remote[B]

  final case class IfElse[A](
    value: Remote[Boolean],
    isTrue: Remote[A],
    isFalse: Remote[A],
  ) extends Remote[A]

  final case class Literal[A](a: A, s: Schema[A]) extends Remote[A]

  final case class AddInteger(first: Remote[Int], second: Remote[Int])
      extends Remote[Int]

  final case class GreaterThan(first: Remote[Int], second: Remote[Int])
      extends Remote[Boolean]

  final case class And(left: Remote[Boolean], right: Remote[Boolean])
      extends Remote[Boolean]

  final case object Length extends Remote[String => Int]

  final case object UpperCase extends Remote[String => String]
}
