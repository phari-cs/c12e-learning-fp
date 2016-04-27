package com.c12e.learn

import com.c12e.learn.data.Nel
import com.c12e.learn.typeclass.Semigroup
// Static wildcard
import com.c12e.learn.typeclass.Semigroup.Syntax._
import com.c12e.learn.stdlib.StringInstances._

// ... these dont have to reference files ...
// Can import objects in package
// import com.c12e.learn.data.\/
import com.c12e.learn.data.\/.Syntax._

object ExampleApp extends App {

  println(implicitly[Semigroup[String]].append("a", "b"))
  // Implicitly comes from stdlib
  // if to rewrite it:
  // def myimplicitly[A](implicit a: A): A = A
  // implicitly grabs a
  // cant have two of the same "kind" of implictys
  // impicit very verbose
  // made apply method so we dont need to call it
  println(Semigroup[String].append("a", "b"))

  // silly, but possible (explicitly passing an implicit parameter)
  println(Semigroup.apply(stdlib.StringInstances.stringMonoid).append("a", "b"))

  println("a" |+| "b")

  println(new Semigroup.Ops("a") |+| "b")

  println(Nel(1,2,3) |+| Nel(4,5,6))

  // Subtype of all types
  // Top types and bottom type
  // Curry howard - expressing false is valid
  // From programmatic perspective, probably dont really intend to express false...
  // Ill allow you to use nothing, but you should use it explicitly
  // Whole point of using disjunction is for having error on left hand side.

  // Any time a function takes parameters, pass it at call time...
  // Explicit type annotation? Explicitly passing type parameter
  println(1.right)
  // flow:
  // // went from:
  // println(implicitly[Semigroup[String]].append("a", "b"))
  // // to this:
  // println(Semigroup[String].append("a", "b"))
  // // to this using syntax:
  // println("a" |+| "b")
  /*
  Good properties for a type class
  --------------------------------
  - global uniqueness of instances for a type
  - lawful (good type classes have laws)
  - multiple instances can be made
  - each function helps derive more functions (data type independently)
  - automatic derivation of new instances
  */


}
