package com.c12e.learn
package data


sealed trait \/[A, B]

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

}

/*
-\/[String, Foo](new Foo)
\/.right[String, Foo](new Foo)
(new Foo).right[String]
*/
