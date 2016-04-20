package com.c12e.learn


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}


class FunctorOps[F[_], A](val fa: F[A]) extends AnyVal {
  def map[B](f: A => B)(implicit ev: Functor[F]): F[B] =
    ev.map(fa)(f)
}


trait FunctorSyntax extends Any {
  implicit def toFunctorOps[F[_] : Functor, A](fa: F[A]) =
    new FunctorOps(fa)
}

object FunctorSyntax extends FunctorSyntax


object Functor {

   import FunctorSyntax._

   def apply[F[_]](implicit ev: Functor[F]): Functor[F] = ev

   trait Laws {

     def identity_[F[_] : Functor, A](fa: F[A]): Boolean =
       // Functor[F].map(fa)(identity) == fa
       fa.map(identity) == fa

     def composition[F[_] : Functor, A, B, C]
         (fa: F[A])(f: A => B)(g: B => C): Boolean =
       fa.map(f).map(g) == fa.map(g compose f)

    }

  }
