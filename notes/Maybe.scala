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

  // def fold[A, B](o: Maybe[A])(ifEmpty: B)(ifJust: A => B): B =
  //   o match {
  //     case Empty() => ifEmpty
  //     case Just(a) => ifJust(a)
  //   }

    // compiles: A Function B
    // compiles: Function[A, B]
    // Doesnt compile: =>[A, B] -> inconsistant, syntactic issues

    // def map[B](f: A Function B): Maybe[B] = {
    // def map[B](f: Function[A, B]): Maybe[B] = {
    // ==>> red black tree
    // \&/ inclusive disjunctions
    // Why do we call map a map - its a funciton, maps keys to values
    // A function just is a map
    // with infix, its keasier to tell what key and value is ...
    // A ==>> B
    def map[A, B](m: Maybe[A])(f: A => B): Maybe[B] = {
      // _ only works for partial applciation when you have one parenthesis
      // demoss ennly miderdern inference algorithm ... uses type of the first arg to infer second arg
      // not good inference - due to java stuff?
      // Figure out where it cant determine what type it is, and tell it ... and mode on
      // Left to right inference
      // Wont work below - sytnatic sugar for _ very parenthesis driven, goes to first ()

      // this.fold(Maybe.empty[B])( Maybe.just(f(_)))
      // syntatic sugar does this: this.fold(Maybe.empty[B])( Maybe.just(x => f(x)))
      // this.fold(Maybe.empty[B])( x => Maybe.just(f(x)))
      // this.fold(Maybe.empty[B])( f andThen Maybe.just)
      // architecture - mathimatical benifit - never do this and youll get this, do this and you get that
      // eta expansion - why do you need this?

      // Point free style - dont point at the variable we are using...
      // Haskell has rich oppertunity for point free, because space composes, and nature of lambda caluclus
      // Not as good in scala - parenthesis
      m.fold(Maybe.empty[B])(x => Just[B](f(x)))


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

    // No real name collision, inside maybe object
    // Traits mix up every singe function from every option - mixed name space.
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

    // How many type parameters does Maybe[A] take: 0
    // How many type parameters does Maybe take: 1
    // Functor takes something that takes something -> Thus Functor[Maybe] is a valid functor
    // F :: A -> B -> C
    // F(a):: B -> C
    // F(a)(b):: C
    //
    // Map:: takes key and value, kind: M[_, _]
    // use kind projectors - partial application of types
    // Map[K, ?]: M[_]
    // Max[K, V]: M
    //
    // _ is special for types -> [_]
    // "Calling" a type is invokeing it with [], and passing args
    // M[_, _] and def M[A, B] have the same kind

    //     if we add    [A, B] here - get overwritten inside the map since we implement them if they are same name. As usefull as making [C, D]
    implicit def functor: Functor[Maybe] = {
      new Functor[Maybe] {
        // [A, B] are part of the definition of map as per the traith
        // The interface gives you the right structure you need.
        // Unversal logical quantification, if you have a maybe of a, and you can unversally may a to b, then you have a maybe of b
        // For all A ,for all B, (Maybe(A) and A -> B) then  Maybe(b)
        // A and B are objects, cant be implication, need predicate for a ->
        // The definition of map on functor requries us to introduce it here

        // Functor for a maybe, irrespective of what it was a maybe of
        // Need this uinversiality

        // map and ap in applicator feel the same
        // it and the apply kinda look the same
        def map[A, B](ma: Maybe[A])(f: A => B): Maybe[B] =
          map(ma)(f)
      }
    }
    // There is a correspondence between logic and types - univeral quantification and exestential quantification.
    // A predicate -> Is green.
    // For all A -> is green A is False
    // There exists an A where a is green. This is true.

    // logitions more rigid for proof structures

}

final case class Empty[A]() extends Maybe[A]
final case class Just[A](a: A) extends Maybe[A]
