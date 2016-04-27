package com.c12e.learn
package data

// Through curry howard not exclusive??
// But there is exclusivity...

// Names can be symbolic
// Can do traits or abstract classes
sealed trait \/[A, B]

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
}
