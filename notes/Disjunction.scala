package com.c12e.learn
package data

// Through curry howard not exclusive??
// But there is exclusivity...

// Names can be symbolic
// Can do traits or abstract classes
import com.c12e.learn.Functor

// Trait can be a typical alpha numeric, or all symbolic
// Special case: No method or object - no analog on the type level for the weird properties of stuff that ends with ":"
sealed trait \/[A, B] {
  def map[C](f: B => C): A \/ C =
    // this match {
    //   // @ sign - put name on inside and rebind it,
    //   //a@-\/(b) => ??
    //   // gonna unwrap to re-wrap ...
    //   // -\/(a) => -\/(a)
    //   // case (a)-\/ => a
    //   case (a)-\/ => this
    //   // Dont have syntax here.
    //   case \/-(b) => \/-(f(b))
    //
    // }
    // fold[A \/ C]( x => x.left[C])(x => f(x).right(A))
    // fold[A \/ C](-\/.apply)(f andThen \/-.apply)
    fold[A \/ C](-\/.apply)(f andThen \/-.apply)
  // we get apply for free from case class

  // fold is curried, foldleft and foldright are not
  // inference reason and performace reason ...
  // if you dont curry, inference problems, inferes one parameter group at a time.
  // if you do curry, additional stuff on the heap?
  def fold[C](ifLeft: A => C)(ifRight: B => C): C =
    this match {
      case -\/(a) => ifLeft(a)
      case \/-(b) => ifRight(b)
    }

  // def fold[C](ifRight: r)(ifLeft: l)(rightFunction: )(leftFunction: ): =
  //   this match {}
}

// A and B not in scope here from above
final case class -\/[A, B](a: A) extends \/[A, B]
// Need to still know what the left type still is, dont loose that info!

// final case class \/-[A, B](b: B) extends \/[A, B]
// This compiles as well ?!?!?!?!?!
final case class \/-[A, B](b: B) extends (A \/ B)
// ... we are already used to do this
// =>[A, B]

object \/ {
  // Not really smart constructor if it doesnt narrow type
  // Want the compiler to infere real type, disjunction
  def left[A, B](a: A): A \/ B = -\/(a)
  def right[A, B](b: B): A \/ B = \/-(b)
  // Works on any object, take object we are enriching, and wrap it!
  // Classes dont have = in def
  // Inference can bite you if you dont have return type specified...
  // Can return "nothing" - kinda hope that nothing is never infered
  final class Ops[A](a: A) {
    def right[B]: B \/ A = \/-(a)
    def left[B]: A \/ B = -\/(a)
  }

  // scala tried to implement an implicit class, cant extend anyval...
  // implicit class Ops[A](val a: A) extends AnyVal {
  //   def right[B]: B \/ A = \/-(a)
  //   def left[B]: A \/ B = -\/(a)
  // }
  // Cant mix in conversions into trait ... kind of can, not as convinient...
  // Scalaz does it verbose -- different reasons - wanted to support tonine or something
  // Implicit not on trait - easier to mix in
  // Implicit class almost mkes:
  // the implicit def is not on a trait and has funny name
  // final class () extends Any {
  // } + funny implicit def not on trait

  // Syntax responsible for wrapping A in Ops
  // (providing implicit function that can covert an A to Ops[A])
  // We can mix sytaxes up, thus we need to make sure names dont collide
  trait Syntax {
      // Our ops doesnt take a Disj, it returns one this A: \/ is not true
      implicit def anyToDisjOps[A](a: A): Ops[A] = new Ops(a)
  }
  object Syntax extends Syntax
  // Want to make it so that we have 1.right[String] ...


    // For compiler to find it, needs to be implicit scope...

    // method vs field is like val to def
    // method gets called every time
    // in scala, vals and defs are in same namespace, but in jvm, they are in different namespaces...
    // Uniform access policy...

    // start with val and see if you need a def.
    // need a def if we have a type or value parameter...

    // pattern, violate rules for namming with typeclasses...
    // its just capitalization...

    // Functor takes something that has one whole ...
    // Disjunction has two holes ... Thus we need to "pin" one of the holes of the disjunction
    // Can functor even be implmeneted for this ...
    // To make sure you can, get implementation and make sure it adheres to laws.

    // We are getting queston mark because of custom plugging, not enabled in raw scala.
    // Here we are making a functor that is fixed in terms of A
    // Disjunction is meant to say that you have a left side and a right side, and the left side by convention is to be an error. Right Baised.

   // When we say F[_] -> we are unifying it with A \/ ?
   // Every time you see F of, we say a \/
   // Called type unification, miles sabine, wrote shapeless,
   // fixed some unification dilemanas, its a problem in scala,
   // unapply pops up in this kind of stuff
   // scala fixed some of this stuff this month
   // defect very well known - SI-2712

  // Very direct correspondence between type and logic.
  // Were making statements that are universally quantified.
    implicit def functor[A] : Functor[ A \/ ? ] = {
      new Functor[ A \/ ? ] {
        def map[B, C](fa: A \/ B)(f: B => C): A \/ C =
          fa map f
      }
    }

}
