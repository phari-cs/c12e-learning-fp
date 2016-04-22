package com.c12e.learn
package stdlib


import com.c12e.learn.typeclass.Monoid


trait TupleInstances {

  implicit def tupleMonoid[A : Monoid, B : Monoid]: Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def append(p1: (A, B), p2: (A, B)) =
        (Monoid[A].append(p1._1, p2._1), Monoid[B].append(p1._2, p2._2))
      def empty = (Monoid[A].empty, Monoid[B].empty)
    }

}


object TupleInstances extends TupleInstances
