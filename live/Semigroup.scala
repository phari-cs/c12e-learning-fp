package com.c12e.learn


trait Semigroup[A] {
   // must be associative:
   // (a append b) append c) === a append (b append c)
   def append(a1: A, a2: A): A
}


trait SemigroupInstances {

   implicit val stringSemigroup: Semigroup[String] =
     new Semigroup[String] {
       def append(a1: String, a2: String) = a1 + a2
     }

   implicit val maxIntSemigroup: Semigroup[Max[Int]] =
     new Semigroup[Max[Int]] {
       def append(a1: Max[Int], a2: Max[Int]) = Max(math.max(a1.a, a2.a))
     }

   implicit def tupleSemigroup[A : Semigroup, B : Semigroup]: Semigroup[(A, B)] =
     new Semigroup[(A, B)] {
       def append(p1: (A, B), p2: (A, B)) =
         (Semigroup[A].append(p1._1, p2._1), Semigroup[B].append(p1._2, p2._2))
     }

}

final class SemigroupOps[A](val a: A) extends AnyVal {
  // method we are adding
  def |+|(a2: A)(implicit ev: Semigroup[A]) = ev.append(a, a2)
}

// thing that adds the functionality
trait SemigroupSyntax {

  // This guy and the guy below are exatly identical
  // implicit def semigroupToOps[A]
  //   (a: A)(implicit ev: Semigroup[A]): SemigroupOps[A] =
  implicit def semigroupToOps[A : Semigroup](a: A): SemigroupOps[A] =
    new SemigroupOps(a)
}


object SemigroupSyntax extends SemigroupSyntax
object Semigroup extends SemigroupInstances {
   @inline def apply[A](implicit ev: Semigroup[A]) = ev
}
