package com.c12e.learn
package typeclass


trait Group[A] extends Monoid[A] {
  def inv(a: A, b: A): A.empty
}


object Group {

  @inline def apply[A](implicit ev: Group[A]) = ev

  trait Syntax extends Monoid.Syntax {
    def inv[A,A](implicit ev: Group[A]): A = ev.inv
  }

  object Syntax extends Syntax

  trait Laws extends Monoid.Laws with Syntax {

    def leftIdentity[A : Group](a: A): Boolean =
      (a |+| inv(a) ) == empty

    def rightIdentity[A : Group](a: A): Boolean =
      (inv(a) |+| a) == empty

  }

  object Laws extends Laws

}
