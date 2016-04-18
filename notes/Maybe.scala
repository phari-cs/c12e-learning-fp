package com.c12e.learn


sealed abstract class Maybe[A] {
  def fold[B](ifEmpty: B)(ifJust: A => B): B =
    this match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }
}

object Maybe {

  // Smart constructors
  def empty[A]: Maybe[A] = Empty()
  def just[A](a: A): Maybe[A] = Just(a)

  def fold[A, B](o: Maybe[B])(ifEmpty: A)(ifJust: B => A): A =
    o match {
      case Empty() => ifEmpty
      case Just(a) => ifJust(a)
    }

    // Is a talks about subtype inheritance - dont generally do
    //[A: Semigroup] -> the semigroup will create a second implicit, guarenteeing that A has a Semigroup implementation
    // - meaning we can find an implementaiton that goes from A -> Semigroup[A]

    // data type is declaration for scemantic intent
    // A : Semigroup is not more of a constraint of A, not really type of A
    // A : Semigroup <: B >: C
    //   ... A has a smigorup and is a subtype of b and supertype of C
    // The COLON here means something else - it doesnt mean type, it means constraint!
    // Process of unifcation -> two things are the same - use the same type variable - substitue it for one of them
    // cant use |+| -> chicken and egg - keep on comming back to yourself...

    // arent using flatmap and map - for sugar uses it, havent gotten to those type classes yet

    import SemigroupSyntax._
    // lets us use |+|

    implicit def maybeSemigroup[A : Semigroup]: Semigroup[Maybe[A]] =
      new Semigroup[Maybe[A]]{
        def append(a1: Maybe[A], a2: Maybe[A]): Maybe[A] = {
          // if a1 is empty, return a2 ..........  if a2 is empty, return a1
          // else append a1 and a2
          // // a1.fold(a2: Maybe[A])( a => a2.fold(a1)( b => Just(Semigroup[A].append(a, b))))
          a1.fold(a2: Maybe[A])( a => a2.fold(a1)( b => Just(a |+| b)))
        }
        // def append(a: Maybe[A], b: Maybe[A]): Maybe[A] = {
        //   a match {
        //     case Empty() => b
        //     case Just(aa) =>
        //       b match {
        //         case Empty() => empty
        //         case Just(bb) => just(aa |+| bb)
        //         // case Just(bb) => just(Semigroup[A].append(aa, bb))
        //       }
        //   }
        // }
      }

}

final case class Empty[A]() extends Maybe[A]
final case class Just[A](a: A) extends Maybe[A]
