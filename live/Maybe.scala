package com.c12e.learn


sealed abstract class Maybe[A] {
  def fold[B](ifEmpty: B)(ifJust: A => B): B =
    this match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }
}

object Maybe {

  def empty[A]: Maybe[A] = Empty()

  def just[A](a: A): Maybe[A] = Just(a)
 
  def fold[A, B](o: Maybe[B])(ifEmpty: A)(ifJust: B => A): A =
    o match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }

  implicit def maybeSemigroup[A : Semigroup]: Semigroup[Maybe[A]] =
    new Semigroup[Maybe[A]]{
      def append(a: Maybe[A], b: Maybe[A]): Maybe[A] = 
        a match {
          case Empty() => b
          case Just(aa) =>
            b match{
              case Empty() => a
              case Just(bb) => just(Semigroup[A].append(aa, bb))
            }
        }
    }
}

final case class Empty[A]() extends Maybe[A]
final case class Just[A](a: A) extends Maybe[A]
