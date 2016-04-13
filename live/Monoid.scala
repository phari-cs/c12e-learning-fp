package com.c12e.learn

trait Monoid[A] {
   // must be associative:
   // (a append b) append c) === a append (b append c)
  def append(a1: A, a2: A): A

  def empty : A
}


trait MonoidInstances {

   implicit val stringMonoid: Monoid[String] =
     new Monoid[String] {
       def append(a1: String, a2: String) = a1 + a2
       def empty = ""
     }

   implicit val maxIntMonoid: Monoid[Max[Int]] =
     new Monoid[Max[Int]] {
       def append(a1: Max[Int], a2: Max[Int]) = Max(math.max(a1.a, a2.a))
       def empty = Max(Int.MaxValue)
     }

   implicit def tupleMonoid[A : Monoid, B : Monoid]: Monoid[(A, B)] =
     new Monoid[(A, B)] {
       def append(p1: (A, B), p2: (A, B)) =
         (Monoid[A].append(p1._1, p2._1), Monoid[B].append(p1._2, p2._2))
       def empty = (Monoid[A].empty, Monoid[B].empty)
     }

}


final class MonoidOps[A](val a: A) extends AnyVal {
  // method we are adding
  def |+|(a2: A)(implicit ev: Monoid[A]): A = ev.append(a, a2)
}


// thing that adds the functionality
trait MonoidSyntax {

  // This guy and the guy below are exatly identical
  // implicit def semigroupToOps[A]
  //   (a: A)(implicit ev: Monoid[A]): MonoidOps[A] =
  implicit def monoidToOps[A : Monoid](a: A): MonoidOps[A] =
    new MonoidOps(a)

  def empty[A](implicit ev: Monoid[A]): A = ev.empty

}


object MonoidSyntax extends MonoidSyntax
object Monoid extends MonoidInstances {
   @inline def apply[A](implicit ev: Monoid[A]) = ev
}
