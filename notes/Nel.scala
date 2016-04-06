package com.c12e.learn

// Have a type ... but cant construct it
// Prove-ably no inhbitants
// Nel is type - value is an inhabitant of the type
// Guarente at compile time that this thing has a value
// Moving stuff from run time to compile time ...

// Interface definition
// Def - all functions are values - more about defining values not functions ...
sealed abstract class Nel[A] { self =>
    def head: A
    def tail: IList[A]

    // A list inherits from seek
    def ++(n: Nel[A]): Nel[A] =
        new Nel[A] { // selfB =>
            def head = self.head
            def tail = self.tail ++ (n.head +: n.tail)
        }

}


// Objects are values
// Dont exist for type constuctor - needs
// object Nel is type Nel.type

// every val are one type
// types are not type constructor

// cant talk about a value of a type constructor
// talk about a saturated value - object exists with all types filled in
// object is singleton - cant talk about it polymorphically

// Rank N encoding.
// Once you put brackets - you introduce a type variable
// ID is a value -> with respect to type parameter


// Prelude - imported by default

// difference between def, object and val -> all values though
// only thing we can talk about polymorphically is a def


// defs and classes can have [...]
// objects and vals cant

// Polymorphic consistensy would have been nice?

// Dont expose type constructor as a type ...
// Tempting - not worth it

object Nel {
    // Minmal needed
    def id[A](a: A) = a
    // Both of these would of worked - type descriptions ...
    // def id[A](a: A): A = a
    // def id[A](a: A): A = a: A
    // Compilers like it when you describe the type of the output of a def


    //applying a seek to tail:
    // Nel(h, t:_*)

    // apply is same as __call__


    def apply[A](h: A, t: A*): Nel[A] = {
        // Tail is going to be a seek - scala std lib aweful type
        // Can run into silly problems - could be mutable - could be immutable - convention - mostly immutable
        // Not why we are using a type system
        // If you throw a seek into apply, it wont work - there is some unwrapping that needs to be done: :_*

        // be careful of recursive call - head and h cant be same name
        new Nel[A] {
            def head = h
            def tail = IList(t:_*) // T is a seek
        }
    }

    // To get semigroup, implement append

}

// x = Array(1,2,3) -> calling apply on companion object
// x(1) -> calling apply on array implementation

// If we unseal, average person can implementation

// A type with no inhabitants///
// Have to give it different name -> classes share same package name space ...
// Cant have files in same package with same case class name
// Not even if we make them private ....
// Can move then into the companion object
//

// If you have a singleton data consturctor - is pattern matching helpful?
//
// case class


// classes and traits, need {} if you have body
// for defs - like if statement - only 1 expression - dont need them.
