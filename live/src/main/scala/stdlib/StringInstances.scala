package com.c12e.learn
package stdlib


import com.c12e.learn.typeclass.Monoid


trait StringInstances {

  implicit val stringMonoid: Monoid[String] =
    new Monoid[String] {
      def append(a1: String, a2: String) = a1 + a2
      def empty = ""
    }

}


object StringInstances extends StringInstances
