package com.c12e.learn
package data


import com.c12e.learn.typeclass.{ Semigroup, Equal, Functor, Applicative }
import com.c12e.learn.typeclass.Equal.Syntax._
import com.c12e.learn.typeclass.Semigroup.Syntax._

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
  import Syntax._

  implicit def applicative[A]: Applicative[A \/ ?] =
    new Applicative[A \/ ?] {

      def pure[B](b: B): A \/ B = b.right[A]

      def ap[B, C](fb: A \/ B)(fbc: A \/ (B => C)): A \/ C =
        fb.fold { _.left[C] } { b => fbc map { f => f(b) } }
        // fb match {
        //   case -\/(a) => a.left[C] // (-\/(a): A \/ B)
        //   case \/-(b) => fbc map { f => f(b) }
        // }

      def map[B, C](fb: A \/ B)(f: B => C): A \/ C =
        fb map f

    }

  implicit def equal[A : Equal, B : Equal]: Equal[A \/ B] =
    new Equal[A \/ B] {
      def equal(d1: A \/ B, d2: A \/ B) =
        d1.fold { a1 =>
          d2.fold { _ === a1 } { _ => false }
        } { b1 =>
          d2.fold { _ => false } { _ === b1 }
        }
    }

    implicit def semigroup[A, B: Semigroup]: Semigroup[A \/ B] =
      new Semigroup[A \/ B] {
        def append(d1: A \/ B, d2: A \/ B) = d1.fold { _.left[B] } { d11 => d2 map { d22 => d11 |+| d22 } }
      }

}
