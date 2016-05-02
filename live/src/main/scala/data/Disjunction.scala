package com.c12e.learn
package data


import com.c12e.learn.typeclass.Functor


sealed trait \/[A, B] {

  def fold[C](ifLeft: A => C)(ifRight: B => C): C =
    this match {
      case -\/(a) => ifLeft(a)
      case \/-(b) => ifRight(b)
    }

  def map[C](f: B => C): A \/ C =
    fold[A \/ C](-\/(_))(f andThen \/-.apply)

}

final case class -\/[A, B](a: A) extends (A \/ B)
final case class \/-[A, B](b: B) extends (A \/ B)


object \/ {

  def left[A, B](a: A): A \/ B = -\/(a)
  def right[A, B](b: B): A \/ B = \/-(b)

  final class Ops[A](val a: A) extends AnyVal {
    def right[B]: B \/ A = \/-(a)
    def left[B]: A \/ B = -\/(a)
  }

  trait Syntax {
    implicit def any2DisjOps[A](a: A): Ops[A] =
      new Ops(a)
  }

  object Syntax extends Syntax

  implicit def functor[A]: Functor[A \/ ?] =
    new Functor[A \/ ?] {
      def map[B, C](fb: A \/ B)(f: B => C): A \/ C =
        fb map f
    }

}

/*
-\/[String, Foo](new Foo)
\/.right[String, Foo](new Foo)
(new Foo).right[String]
*/
