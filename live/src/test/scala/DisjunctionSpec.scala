package com.c12e.learn
package test


import com.c12e.learn.data.{ \/, IList }
import com.c12e.learn.data.\/.Syntax._
import com.c12e.learn.typeclass.Semigroup.Syntax._
import com.c12e.learn.stdlib.instances._


object DisjunctionSpec extends Spec("disjunction") {
  checkAll(Props.equal[String \/ Int])
  checkAll(Props.semigroup[String \/ IList[Int]])
  checkAll(Props.applicative[String \/ ?, Int, String, (Int, String)])

  "Just want to print the value of the disjunction semigroup" in {
    println(IList(1).right[String] |+| IList(2).right[String])
    true
  }

}
