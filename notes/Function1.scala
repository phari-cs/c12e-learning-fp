package com.c12e.learn
package stdlib
package instances


// import com.c12e.learn.typeclass.Functor
import com.c12e.learn.typeclass.Applicative


trait Function1Instances {

  // implicit def function1Functor[A]: Functor[A => ?] =
  //   new Functor[A => ?]{
  //     def map[I, J](fi: A => I)(f: I => J): A => J =
  //       f compose fi
  //   }

  // save you boiler plate when you have to thread same value over and over and over again...
  // gobal config
  implicit def function1Applicative[A]: Applicative[A => ?] =
    new Applicative[A => ?]{
      def pure[I](i: I): A => I =
        a => i
      def ap[I, J](fi: A => I)(fij: A => I => J): A => J =
        a => fij(a)(fi(a))
      def map[I, J](fi: A => I)(f: I => J): A => J =
        f compose fi
    }

// si2717 - defect that doesnt allow us to do sytnac
// unificatin algorithm not complete - just stops
// have to do long had version to call map for function1 instances
// miles is putting in a fix, same dude that wrote shapeless

}


object Function1Instances extends Function1Instances
