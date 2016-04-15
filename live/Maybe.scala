package com.c12e.learn


sealed abstract class Maybe[A] {
  def fold[B](ifEmpty: B)(ifJust: A => B): B =
    this match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }
}

object Maybe {
  def fold[A, B](o: Maybe[B])(ifEmpty: A)(ifJust: B => A): A =
    o match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }
}

final case class Empty[A]() extends Maybe[A]
final case class Just[A](a: A) extends Maybe[A]
