package com.c12e.learn

// Functor

// Functors
// Inspired by category theory, but not fully category theory

// F is for functor
// only going to have one method on it
// Needs to know that F[_] because we use it with F[A] later
// The _ means dont assign any variables to this type.
// F has a functor ..., the F is representing the data type that has a functor

// Terminology ->

// Laws:
// Identity and Composition


// List implemented semigroup in companion object
// Functor is a typeclass in terms of something that takes a type prameter


trait Functor[F[_]] {
  // some other places call it fmap - for functor maps, because they have different maps
  // generally, when we have functions comming in, they are labeled with f
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// val was for optimication, so was anyval
// wrap fa in Functor ops
class FunctorOps[F[_], A](val fa: F[A]) extends AnyVal {
  // implicit block has to be last
  def map[B](f: A => B)(implicit ev: Functor[F]): F[B] =
    ev.map(fa)(f)
}

// typeclass enrichments is first way to use implicits
// implicit conversion is second way to use implicits
trait FunctorSyntax {
    implicit def toFunctorOps[F[_]: Functor, A](fa: F[A]) =
      new FunctorOps(fa)
}


// mix traits together to comibne traits, then make objet out of it.
// mixing up sytax - can use inheritance

// Need this to be able to import
object FunctorSyntax extends FunctorSyntax

// open recursion - use something in the process of defining it
object Functor {

  // imports are only allowable on objects, (on values) - make companion object so we can import from syntax
  import FunctorSyntax._
  // Dont use [F[_] : Functor] because we want to return the evidence
  // Functor needs F[_] c
  //     you have a type of something - exists for java compatibility - for ?
  //     here you introduce type param
  //                             exestential type when on the right side, you use F[_] for ex
  def apply[F[_]](implicit ev: Functor[F]): Functor[F] = ev

  trait Laws {
    // Only constraining the int..., the A doesnt matter
    def notDefaultIdentity[F[_]: Functor, A](fa: F[A]): Boolean =
      fa.map(identity) == fa

    def composition[F[_] : Functor, A, B, C]
          (fa: F[A])(f: A => B)(g: B => C): Boolean =
            fa.map(f).map(g) == fa.map(g compose f)
    // Scalaz has laws in production source code, not even in test code
  }

  implicit def func1Functor[C] : Functor[ C => ?] =
    new Functor[C => ?] {
      def map[A, B](fa: C => A)(f: A => B): C => B =
        a => f(fa(a))
        // fa andThen f
        // f compose fa
    }

  // You have something that ends with a b... it takes an a and ends with a b,
  // so we need a functor of a type that when its filled, it can go

  // The other way wont work! The whole has to be on the right because of function applicaiton...
  // We might have a reverse engineered typeclass?
  // implicit def func1Functor[C] : Functor[? => C] =
  //   new Functor[? => C] {
  //     def map[A, B](fa: A => C)(f: A => B): B => C =
  //       b =>
  //   }
  // implicit def func1Functor[A] : Functor[Function1[A, ?]]

}


// What does it mean to implement functor....
