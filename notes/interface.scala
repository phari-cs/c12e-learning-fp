package com.c12e.learn


// defs and vals are interchangable
// def vs val - recalculated vs not
// difference between def and val alot less interesting when doing fp
trait PointI {
  def x: Int
  def y: Int
}

// when we define it as a method - on stack
// first class funciton - more on the heap

object PointI {

  def apply(_x: Int, _y: Int): PointI =
    // anonymously implementing the interface ... not giving it a name
    new PointI {
      def x = _x
      def y = _y
    }

}


// which design space do you use, up or down
// point has one data constructor
sealed abstract class PointADT {

  def fold[A](f: (Int, Int) => A): A =
    this match {
      case PointDC(x, y) => f(x, y)
    }

  def x: Int = fold((x, y) => x)
  def y: Int = fold((x, y) => y)

}
// Data constructor are values not type, can have many
// What do we want the type to be then - values have only one type - this is what seperate type theory from set theory
//               heres a value                         of this type
// DONT think of it like subtyping! (only doing this because of the jvm)
final case class PointDC(xVal: Int, yVal: Int) extends PointADT
// If you have a pointadt and what to pull out xval - need to pattern match
// Incredible amount of transperancy with adts - we access the exact values we pass
// With object oriented stuff, we rely on the hiding, when we access something, it could have gone throught a tranformation...

// WIth the fold - we have the full expressivity of the Object Oriented PointI
// ... could have folded PointI on the PointADT to prove it


// We make an apply since we only have one data constructor
object PointADT {
  def apply(x: Int, y: Int): PointADT = PointDC(x, y)
}


// sum type is the alternates --- the cases for the same type
// the interface is just a product type ...

class PointC(val x: Int, val y: Int)

object PointC {
  def apply(x: Int, y: Int): PointC =
    new PointC(x, y)
}


// if i have an interface -- all i need is a class that takes functions, a class is just as powerful
// almost never use class
// if you have alot of first class funcitons - make them a trait? - why?
// boolean is the simplist data constructor

// w/o bools, implement boolean
// not good with interface design ... slip into visitor pattern for oo person...
// just a class with 2 types ... fp

final case class Point(x: Int, y: Int) // Data constructor same name as type - only one - wont hurt us, similar to class


object PointUsage {
  val pi = PointI(1,2)
  val padt = PointADT(1,2)
  val pc = PointC(1,2)
  val p = Point(1,2)

  println(s"${pi.x}, ${padt.x}, ${pc.x}, ${p.x}")
}

// if you have adt that takes no params,
//   ... and they are not polymorphic with type variable emoty list of int is dif than empty list of string (doesnt matter with type erasure)
// then you use case objects

// Start with an algebraic data type and crop it down!
// if you have one data constructor its easier to trim donw

// case object x - has x.type
// an if is a fold on a language specified bool datatype

// Non eager doenst always mean lazy - smaller contract-  lazy is one way of implementing - non eager means you dont know if its implemented or note
// strict i.e eager - implemented before passed in
// lazy only done once
// by name - every time is referenced (not memoized)
// byname to lazy - call once and cache

// a case class with functions is an interface
// an interface is a giant bag of funcitons

// implement bool in oo to see frustration
