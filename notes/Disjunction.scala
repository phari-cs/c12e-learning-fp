package com.c12e.learn
package data

// Names can be symbolic
// Can do traits or abstract classes
trait \/[A, B]

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
}
