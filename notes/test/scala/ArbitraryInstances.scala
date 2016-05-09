package com.c12e.learn
package test


import org.scalacheck.{ Arbitrary, Gen }

import com.c12e.learn.data._
// import com.c12e.learn.data.{ Max, Maybe }
import com.c12e.learn.typeclass.{ Applicative, Functor }
import com.c12e.learn.typeclass.Functor.Syntax._


// ran a 100 tests for each
// copied scalaz test framework implementation
// identity is tested 100 times
// John Hues, started off in Haskell, quick check, ported to quorta test -> Reed draper -> worked in erlang -> John wrote the erlang one -> they know each other

// Ricky Nilson ported quick check stuff to Scala check … same stuff.
// The red book: don’t get overwhelmed by the idea that you need a testing library… You can write it from scratch.
// Scala test is much smaller than Scalaz
// This is only Scala check …
// gets rid of some boiler plate in Ricky’s library
// teach arbitrary on how to build stuff of custom type.
// It can build common stuff for free.
// If you can map from something it can generate to something it cant, then we can make a arbitrary generator of a custom type.
// It seams life arbitrary is a functor, but its an applicative as well.
// when we realize that we can use a type class instance for arbitrary, get syntax for free.


// checkall props from other file, run them in framework
// running properties within properties
// we are making a test suite

// gen is more the data type
// and arbitrary for typeclass
// thus we have global uniqueness
// good generators have a random distribution

// arbirtary is one and only one for this type.
// we can have multiple generators for the same type

trait ArbitraryInstances {

  implicit def genApplicative: Applicative[Gen] =
    new Applicative[Gen] {
      def pure[A](a: A) = Gen const a
      def map[A, B](ga: Gen[A])(f: A => B) = ga map f
      def ap[A, B](ga: Gen[A])(gf: Gen[A => B]) =
        gf flatMap { f => ga map { a => f(a) } }
    }

  implicit def arbitraryApplicative: Applicative[Arbitrary] =
    new Applicative[Arbitrary] {
      def pure[A](a: A) =
        Arbitrary(Applicative[Gen].pure(a))
      def map[A, B](a: Arbitrary[A])(f: A => B) =
        Arbitrary(Functor[Gen].map(a.arbitrary)(f))
      def ap[A, B](a: Arbitrary[A])(f: Arbitrary[A => B]) =
        Arbitrary(Applicative[Gen].ap(a.arbitrary)(f.arbitrary))
    }

  implicit def maxArbitrary[A : Arbitrary]: Arbitrary[Max[A]] =
    arb[A] map Max.apply

  // wanted to have the right probability of testing stuff ... dont want to test none a whole crap ton of times.
  implicit def maybeArbitrary[A : Arbitrary]: Arbitrary[Maybe[A]] =
    Arbitrary(Gen.sized(n =>
      // When n is larger, make it less likely that we generate None,
      // but still do it some of the time. When n is zero, we always
      // generate None, since it's the smallest value.
      Gen.frequency(
        (n, Gen.resize(n / 2, Arbitrary.arbitrary[A]).map(Maybe.just[A])),
        (1, Gen.const(Maybe.empty)))))

  // Either sucks ... same structure as either... no functions, not right biased, no type classes. No body likes it.
  // Since either and disjunction is isomorphic - have same structure, no weird varience annotation
  // Because either and disjunction are isomorphic, we can bootsrap off of either
  implicit def disjunctionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A \/ B] =
    arb[Either[A, B]] map { _.fold(\/.left)(\/.right) }

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]
  // could have done as well:
  // private def arb[A](implicitly ev: Arbitrary[A]): Arbitrary = ev

// Write properties in testing framework as I have in production framework...
// Take prewritten laws and wrote them in regards to for all.
}


object ArbitraryInstances extends ArbitraryInstances
