package com.c12e.learn
package test

import com.c12e.learn.typeclass.Applicative.Syntax._
import com.c12e.learn.data.IList
import com.c12e.learn.stdlib.instances._

// // Essentially, we start off by testing our typeclasses
object IListSpec extends Spec("ilist") {
  checkAll(Props.equal[IList[String]])
  checkAll(Props.semigroup[IList[Int]])
  checkAll(Props.applicative[IList, Int, String, (Int, String)])

  "printing applicative" in {
    println(IList(1, 2, 3) <*> IList({x: Int => x + 1}, {x: Int => x * 10}))
    true
  }


  "making sure zip works right" in {
    val actual = IList(1, 2, 3).zip(IList(4,5))
    val expected = IList((1, 4), (2, 5))
    println("actual: " + actual)
    println("expected: " + expected)
    actual must_=== expected
  }
}