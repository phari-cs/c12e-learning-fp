package com.c12e.learn
package data


import com.c12e.learn.typeclass.Semigroup


sealed abstract class Maybe[A] {

  def fold[B](ifEmpty: B)(ifJust: A => B): B =
    this match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }

}


final case class Empty[A]() extends Maybe[A]
final case class Just[A](a: A) extends Maybe[A]


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
        a.fold(b) { aa =>
          b.fold(a) { bb =>
            just(Semigroup[A].append(aa, bb))
          }
        }
    }

}
