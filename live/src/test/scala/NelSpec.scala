package com.c12e.learn
package test

// import com.c12e.learn.typeclass.Applicative.Syntax._
import com.c12e.learn.data.Nel
import com.c12e.learn.stdlib.instances._

// Essentially, we start off by testing our typeclasses
object NelSpec extends Spec("nel") {
  checkAll(Props.equal[Nel[String]])
  checkAll(Props.semigroup[Nel[Int]])
  // checkAll(Props.monoid[Nel[Int]])
  // TODO: make the following test not blow stack
  // checkAll(Props.applicative[Nel, Int, String, (Int, String)])

  // "Just want to print the value of the appending" in {
  //   println(Nel(1, 2, 3).append(Nel(4,5,6)))
  //   true
  // }
  //
  // "Just want to print the value of the applicative" in {
  //   println(Nel(1, 2, 3) <*> Nel({x: Int => x + 1}, {x: Int => x * 10}))
  //   true
  // }
  //
  // "A simple check for applicative" in {
  //   val actual = Nel(1, 2, 3) <*> Nel({x: Int => x * 10}, {x: Int => x * 100})
  //   val expected = Nel(10, 100, 20, 200, 30, 300)
  //   println("actual: " + actual)
  //   println("expected: " + expected)
  //   actual must_=== expected
  // }
  //
  // "Making sure zip works right" in {
  //   val actual = Nel(1, 2, 3).zip(Nel(4,5))
  //   val expected = Nel((1, 4), (2, 5))
  //   println("actual: " + actual)
  //   println("expected: " + expected)
  //   actual must_=== expected
  // }
}
