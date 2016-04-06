package com.c12e.learn


// Need an objet to put functions on because of JVM
object Curry extends App{

   def add(a: Int, b: Int): Int =
       a + b

   def addCurry(a: Int)(b: Int): Int =
       a + b


  // Similar to :
   // def addCurryManual(a: Int): Function1[Int, Int] =
   def addCurryManual(a: Int): Int => Int =
      { b => a + b }

   // Creating first class functions - > objects on heap
   // Tail rec doenst work for first class objects
   // Only on defs
   // Can write whole program like this, bull will take a performance hit
   // JVM optimizations that we need, thats why we use defs
   // To get a function, need to make a special Function1 class
   val addHof: Int => (Int => Int) =
      { b => { a => a + b } }

    //   addHof(1)
    //   { a => a + 1 }
    //   addHof(1)(2)
    //   2 + 1

   // In apps case ... it is called every time main is invoke, this guy will print
   // Any object can have arbitrary lines of code, and called when constructor is called
   // ... Delayed Initialization

   println(add(1,2))
   println(addCurry(1)(2))
   println(addCurryManual(1)(2))
   println(addHof(1)(2))

   // 7 uses of underscore = kind noteation, exestnetial notations, wildcards, partial evaluation
   // Eta expansion can look like Partial application
   val addPartialOne = add(1, _: Int) // Eta expansion?
   val addCurryOne = addCurry(1)_ // Partial application
   // equvalent?:  addCurry(1)(_) // Eta expansion turns a method / function into a firstclass function
   val addCurryManualOne = addCurryManual(1)
   val addHofOne = addHof(1)

   println(addPartialOne(100))
   println(addCurryOne(100))
   println(addCurryManualOne(100))
   println(addHofOne(100))


  // Curry can only be implemented one way -> Onec Inhabited
   def curry[A, B, C](f: (A, B) => C): A => B => C =
       a => b => f(a, b)
    //    { b => { a => f(a, b) } } Will not compile!
   def uncurry[A, B, C](f: A => B => C): (A, B) => C =
       (a, b) => f(a)(b)

   def flip[A, B, C](f: A => B => C): B => A => C =
       b => a => f(a)(b)

// speakings about funcitons parametrically - get once inhabitant function

  // Cant introduce type variables on vals - need defs - no polymorphic stuff
  // Are fields generic - no
  // Are methods? - yes
  // Parenthesis are needed! Very ambiguous if not there

  // Eta expansion - i know this is a method but i want to treat is as function one
  // Lots of rules of when the _ is needed - is it infered or is it not
  def curryHof[A, B, C]: ((A, B) => C) => (A => B => C) =
      f => a => b => f(a, b)
      //(f: (A, B) => C)=> (a: A) => (b: B) => f(a, b): C

}
