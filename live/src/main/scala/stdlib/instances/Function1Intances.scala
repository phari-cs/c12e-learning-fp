package com.c12e.learn
package stdlib
package instances


import com.c12e.learn.typeclass.Functor


trait Function1Instances {

  implicit def function1Functor[A]: Functor[A => ?] =
    new Functor[A => ?]{
      def map[I, J](fi: A => I)(f: I => J): A => J =
        f compose fi
    }

}


object Function1Instances extends Function1Instances
