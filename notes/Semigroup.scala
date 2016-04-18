package com.c12e.learn


// groups - identity and inverse opperations
// monoid - identity and associative opperations


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

// This [A] is important! We cant put an [B] on append -> append[B]
// Cant change signature of trait

trait Semigroup[A] {
    // must be associative a append b append c = ((a app b) append) = (a append (b append c))
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


// code organization
// implicit resolution and precendence orders
// search and find before it finds anything else
// search lower in the tree first
// resolution
trait SemigroupInstances {

}


// object Semigroup {
// Do this to enable syntax
object Semigroup extends SemigroupInstances{

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

    //////////////////////////////[A: Semigroup, B: Semigroup] - context bound
    ///////////////////////////// desugars into
    // implicit val tupleSemigroup[A, B](impilcit ev1, Semigroup[A], ...): Semigroup[(A, B)] = {
    // only works for something that only has one block

    // This is syntax sugar, where :> is to imply inhereitance
    // only works with defs,
    // if its a val, cant use type parameters at all
    implicit def tupleSemigroup[A: Semigroup, B: Semigroup]: Semigroup[(A, B)] = {
        new Semigroup[(A, B)] {
            // Since apply is implicit, Semigroup[A] goes and pulls that semigroup...
            def append(p1: (A, B), p2: (A, B)): (A, B) = (Semigroup[A].append(p1._1, p2._1), Semigroup[B].append(p1._2, p2._2))
        }
    }
}

// Has to be a class
// Traits cant take parameters
// Extend anyval from warrapping in SemigroupSyntax just to stick a function on it
// Val makes a public val
// Could have been called SemigroupOps as well
// there is no point extending it, everything is called implicitly ... thats why its final

// doesnt use implicit class because it doesnt work with anyval
final class SemigroupOps[A](val a: A) extends AnyVal {
    // Feature called implicit classes - dont work well with boxing
    // Non implicit class way

    // different ++ -> only on list...
    // this will exist on all semigroups
    def |+|(a2: A)(implicit ev1: Semigroup[A]) = ev1.append(a, a2)
}

// Mix trait into package objects
trait SemigroupSyntax {
    implicit def semigroupToOps[A: Semigroup](a:A): SemigroupOps[A] =
        new SemigroupOps(a)
}

// Lets us import it ... cant import trait
object SemigroupSyntax extends SemigroupSyntax

object ExampleApp extends App {
    println(Semigroup.Pi)

    // Both are about same, but line about makes it so that we dont need it...
    println(implicitly[Semigroup[String]].append("a", "b"))
    println(Semigroup[String].append("a", "b"))

    // explicitly pass an implicit parameter
    println(Semigroup.apply(Semigroup.stringSemigroup).append("a", "b"))
}

// console ...

// import com.c12e.learn_
// import com.c12e.learn.Semigroup
// import com.c12e.learn.SemigroupSyntax._
//
// "sdsds" |+| "sdsds"
// List("asdasd") |+| List("asdsadas")
// // Fails = treats |+| as getting two args on right side
// (("c", "d"), IList.cons("e", IList.nil)) |+| (("a", "b"), IList.nil[String])
// // Wrap two args with paren...
// (("c", "d"), IList.cons("e", IList.nil)) |+| (((("a", "b"), IList.nil[String])))


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
