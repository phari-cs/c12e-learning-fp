package com.c12e.learn


// groups - identity and inverse opperations
// monoid - identity and associative opperations


// "New typing"

// AnyVal - makes it so that its not a performance hit
// Dont box - Value Classes / New Types
// Extending case classes is a defect - compiler forces you to make it final

final case class Max[A](a: A) extends AnyVal
// case classes makes setters and getter, constructors

// Interfaces (very close to abstract class)
// trait

// Classes - dont really use them unless try to hide implementation like conventional classes
// class

// scala has multiple inheritence
// case class

// java - dynamic dispatch
// scala - static dispatch

// compliler proofs ... pretty cool

// monoid - from abstract algebra


// similar to make it an instances of comparable vs giving it an instance of comparator
// Bershin Myer -> open to extension, closed to modification - open close principle

// TODO: Lookup what mathematical definition for Monoid and Semigroup is
// This dude is a type

// @ImplicitsNotFound("need a semigroup typeclass")I
trait Semigroup[A] {
    // must associative a append b append c = ((a app b) append) = (a append (b append c))
    def append(a1: A, a2: A): A
}

// if you change an interface, you have to effectively change everything that uses the interface
// Companion object (singleton)
// This dude is a value
// Dont really use object types ....
// JVM: can only hang functions off of classes
// This is where you put your statics
// Static stuff that arent constants and traits
// Objects - a class with a singleton instance
// Do I have to call it the same name as the trait for companion object
// 	yes ... to make implicits work
// implicits make sense for 2 things: type class encoding, and type enrichments
// never do with implicits - can break proofs - for comiletime proofs
// dependency injection (will fail because we are grabbing implicits from global namespace - cant have a )
// 	(juice is some dependency injection framwork)

// cardinal rules
// dont do infinite recursion
// never throw an exception (total functions) (every value in domain maps to a value in the codomain)
// partial functions (functions that can explode)
// no side effects (the solution is delayed execution)

// new on classes and traits

object Semigroup {

    // similar to __call__
    // Before this line Semigroup() would return nothing
    // I can five you an answer but i need a proof that you
    // looks for typeclass companion object and companion object companion objects, thus same name is needed)
    // companion objects - places to put evidence of implementation
    @inline def apply[A](implicit ev: Semigroup[A]) = ev

    val Pi = 3.14
    // Need implicit here
    implicit val stringSemigroup: Semigroup[String] =
        new Semigroup[String] {
            def append(a1: String, a2: String) = a1 + a2
        }
    implicit val maxSemigroup: Semigroup[Max[Int]] =
        new Semigroup[Max[Int]] {
            def append(a1: Max[Int], a2: Max[Int]) = Max(math.max(a1.a, a2.a))
        }
}

object ExampleApp extends App {
    println(Semigroup.Pi)

    // Both are about same, but line about makes it so that we dont need it...
    println(implicitly[Semigroup[String]].append("a", "b"))
    println(Semigroup[String].append("a", "b"))

    // explicitly pass an implicit parameter
    println(Semigroup.apply(Semigroup.stringSemigroup).append("a", "b"))
}

/*
Good Properties for a type class
- global uniqueness (one implementation in the implicit scope)
- lawful (good type classes have laws)
- multiple instance can be made
- each function has lots of derivable operations (each function helps derive more functions (data type independently))
- automatic derivation of new instances list of a list of a list of a list of something - dont
- curry howard correspondence - logic programming at the type level (*type* classes, a class that takes a type that spits out another class)
*/

// Scala has different namespaces for types and values
