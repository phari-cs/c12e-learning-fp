package com.c12e.learn
package data


import com.c12e.learn.typeclass.{ Applicative, Semigroup, Equal }
import com.c12e.learn.typeclass.Applicative.Syntax._
import com.c12e.learn.typeclass.Semigroup.Syntax._
import com.c12e.learn.typeclass.Equal.Syntax._


sealed abstract class Nel[A] { self =>

  def head: A

  def tail: IList[A]

  def ++(that: Nel[A]): Nel[A] = {
    new Nel[A] {
      def head = self.head
      def tail = self.tail ++ (that.head +: that.tail)
    }
  }

  def map[B](f: A => B) =
    new Nel[B] {
      def head = f(self.head)
      def tail = self.tail map f
    }

  def fold[B](ifnil: B)(f: (A, B) => B): B =
    (self.head +: self.tail).fold(ifnil)(f)


  def foldLeft[B](ag: B)(f: (B, A) => B): B =
    (self.head +: self.tail).foldLeft(ag)(f)

}


object Nel {

  def apply[A](h: A, t: A*): Nel[A] =
    new Nel[A] {
      def head = h
      def tail = IList(t: _*)
    }

  implicit def semigroup[A]: Semigroup[Nel[A]] =
    new Semigroup[Nel[A]] {
      def append(a1:Nel[A], a2:Nel[A]) : Nel[A] =
        a1 ++ a2
    }

  implicit def equal[A : Equal]: Equal[Nel[A]] =
    new Equal[Nel[A]] {
      def equal(l1: Nel[A], l2: Nel[A]) =
        l1.head === l2.head && l1.tail === l2.tail
    }

  implicit val applicative: Applicative[Nel] =
    new Applicative[Nel] {
      def pure[A](a: A): Nel[A] = Nel(a)
      def ap[A, B](fa: Nel[A])(fab: Nel[A => B]): Nel[B] =
        new Nel[B]{
          def head = fab.head(fa.head)
          def tail = ((fab.tail map {x => x(fa.head)}) |+| (fa.tail <*> fab.tail))
        }
      def map[A, B](a: Nel[A])(f: A => B): Nel[B] =
        a map f
    }

}
