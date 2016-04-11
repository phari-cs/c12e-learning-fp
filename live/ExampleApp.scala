package com.c12e.learn


import SemigroupSyntax._


object ExampleApp extends App {

  println(implicitly[Semigroup[String]].append("a", "b"))
  println(Semigroup[String].append("a", "b"))

  // silly, but possible (explicitly passing an implicit parameter)
  println(Semigroup.apply(Semigroup.stringSemigroup).append("a", "b"))

  println("a" |+| "b")

  println(new SemigroupOps("a") |+| "b")

  println(Nel(1,2,3) |+| Nel(4,5,6))

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
