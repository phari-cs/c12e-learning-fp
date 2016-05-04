package com.c12e.learn
package typeclass

// Extends fuctor, have map as well
trait Applicative[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A]
  // List has an applicative instance...
  //
  def ap[A, B](fa: F[A])(fab: F[A => B]): F[B]

}


object Applicative {
  // call call.pure
  @inline def apply[F[_]](implicit ev: Applicative[F]): Applicative[F] = ev

  class Ops1[A](val a: A) extends AnyVal {
    def pure[F[_]](implicit ev: Applicative[F]): F[A] = ev pure a
  }
  // Called the eye (of sauron) ...
  // SOme of parenthesis get called cat, butt, ...
  // operators we can call in an object oriented style
  class Ops2[F[_], A](val fa: F[A]) extends AnyVal {
    def <*>[B](f: F[A => B])(implicit ev: Applicative[F]): F[B] = ev.ap(fa)(f)
  }

  trait Syntax extends Functor.Syntax {

    implicit def toApplicativeOps1[A](a: A): Ops1[A] =
      new Ops1(a)

    implicit def toApplicativeOps2[F[_] : Applicative, A]
        (fa: F[A]): Ops2[F, A] =
      new Ops2(fa)

  }

// Going into category theory

// homomorphism is saying we can go to B then F[B],
// Or F[A] to F[B], its our choice, two different paths, same result.

//                      |
//                      |
//            V ---- Pure[F] ---- V
//          A * --------|-------> * F[A]
//            |         |         |
//            |         |         |
//            V ---- Pure[F] ---- V
//     A -> B * --------|-------> * F[A -> B]
//            |         |         |
//            |         |         |
//            V ---- Pure[F] ---- V
//          B * --------|-------> * F[B]
//                      |
//                      |
//                      |

// map fusion, replaces double map with map of composition. Almost default by haskell.

// Applicative, why is it called pure (some people call it return as well) (another label of pure is point)
// - unit, pure, point, return
// - point is offset by co-point: F[A] -> A
// - Sometimes cant go back: go from int to list, but if the list is empty, cant go back to exact int that had the

// Can I service it, not the exact service, dont think of values, what does your code support?
// Less production space, more design space
// Just the existiance of this functionality

// More inhabited for Marketing => $ than we are MagicML => $ :p
// From a desing standpoint, we can start to refine our types... from marketing to ethical marketing...

  object Syntax extends Syntax

  // Dont really use the laws, just documentation
  // We extend functor laws
  object Laws extends Functor.Laws with Syntax {

    // Functor identity - map identity, get back what I started with...
    // you called .pure
    // you have something from a -> a
    // look at syntax in score
    // we have something that converts as  and f[a]s (in the ops above)
    //   ... but it needs an f that is implicitive
    // start with a function that goes from a -> a and "lifting" it to somethign that goes from F[a->a]

    def applicativeIdentity[F[_] : Applicative, A](fa: F[A]) =
      (fa <*> (identity[A] _).pure[F]) == fa

    // a.pure[F] will give us an F[A]
    // ab.pure[F] will give us an F[A => B]
    // And finally on the left side, we :
    //    a.pure[F] <*>  ab.pure[F]
    //  = F[A] <*> F[A => B]
    // <*> will call ap on F[A] passing F[A => B], returning F[B]

    // On the right side, we call ab on a giving us a B,
    // then we call .pure[F] on the B to get a F[B], the same as the left side.
    def homomorphism[F[_] : Applicative, A, B](a: A, ab: A => B) =
      (a.pure[F] <*> ab.pure[F]) == ab(a).pure[F]

    // changed up the order of the function
    // We have to have an A to make this happen, one of them is not pured up
    def interchange[F[_] : Applicative, A, B](a: A, f: F[A => B]) =
      (a.pure[F] <*> f) == (f <*> { (ff: A => B) => ff(a) }.pure[F])

    // Going from an F[A] to an F[B] to an F[C] is the same as
    // We combine F[A-B] with F[B->C] to get an F[A->C] than use it with an F[A] to get another F[C]
    def applicativeComposition[F[_] : Applicative, A, B, C]
        (fa: F[A], fab: F[A => B], fbc: F[B => C]) = {
      val lhs = (fa <*> fab) <*> fbc
      val rhs =
        (fa <*>
          (fab <*>
            (fbc map { (bc: B => C) => (ab: A => B) => bc compose ab })))
      lhs == rhs
    }

    def applicativeDerivedMap[F[_] : Applicative, A, B](f: A => B, fa: F[A]) =
      fa.map(f) == (fa <*> f.pure[F])

  }

}

// laws of equality
// reflexive: a === a
// symetry: a === b same as b === a
// transitivity: if a === b, and b === c, a === c

// congruence is an equavalence relation

// if you can define a function that takes two variables and abides by all of the laws above, then it is an equivalence relation

// Any relation that satisfies all three of these rules is an equivalence relation...
// Might not be the relation we are looking for.

// Java is reference equality, if they occupy same memory, then they are equal
// "hello" == "hello" - false in Java, true in scala.
// Scala overrode it to call .equals

// MEtric spaces and topolgy spaces are non transitive, using them for equals
// Once your domain fits the laws, you get a library for free!

// Problem of matching relates to *Unification*
