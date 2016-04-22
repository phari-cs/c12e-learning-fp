package com.c12e.learn
package typeclass


trait Monoid[A] extends Semigroup[A] {
  def empty : A
}


object Monoid {

  @inline def apply[A](implicit ev: Monoid[A]) = ev

  trait Syntax extends Semigroup.Syntax {
    def empty[A](implicit ev: Monoid[A]): A = ev.empty
  }

  object Syntax extends Syntax

  trait Laws extends Semigroup.Laws with Syntax {

    def leftIdentity[A : Monoid](a: A): Boolean =
      (empty |+| a) == a

    def rightIdentity[A : Monoid](a: A): Boolean =
      (a |+| empty) == a

  }

  object Laws extends Laws

}
