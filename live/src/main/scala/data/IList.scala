package com.c12e.learn
package data


import scala.annotation.tailrec

import com.c12e.learn.typeclass.{Monoid, Applicative, Equal}
import com.c12e.learn.typeclass.Equal.Syntax._

sealed abstract class IList[A] {

  // trampolining can turn something writen in non tailrec to something in tailrec position
  // use cps to do something like this - expensive in scala - puts stuff on heap because stack isnt optomized for this or something - slower
  // this kind of stuff lifts the function into a lambda, and does stuff on the heap instead of the stack
  def fold[B](ifNil: B)(ifCons: (A, B) => B): B =
    this match {
      case INil() => ifNil
      case ICons(h, t) => ifCons(h, t.fold(ifNil)(ifCons))
    }

  // stack and heaps:

  // -------------------------
  // Loaded Code
  // -------------------------
  // Stack (physically allocated right after code usually)
  // - stack computations faster, more efficient than heap
  // - limit of like 1000 things on stack or something...
  // - used every time a function is called.
  // - inputs, functions, and outputs of function are stored on stack
  // -------------------------
  // Heap (much larger than stack)
  // - much larger than stack
  // -------------------------

  // every time a function calls itself, it has the stuff that called it on the stack, and the stuff its calling itself with on the stack as well.
  // this dude will make it so that the previous recrsive call for this function gets poped and replaced with the next call of the same funciton.
  // Only have one of this functions call on the stack
  @tailrec
  def foldLeft[B](ifNil: B)(ifCons: (B, A) => B): B =
    this match {
      case INil() => ifNil
      case ICons(h, t) => t.foldLeft(ifCons(ifNil, h))(ifCons)
    }

  @tailrec
  def foldl2[B, C](l2: IList[B])(ifNil: C)(ifCons: (C, A, B) => C): C =
    this match {
      case INil() => ifNil
      case ICons(h, t) => l2 match {
        case INil() => ifNil
        case ICons(h2, t2) => t.foldl2(t2)(ifCons(ifNil, h, h2))(ifCons)
      }
    }

  // implement fold left in terms of fold right
  def longZip[B](l2: IList[B]): IList[(Maybe[A], Maybe[B])] =
    this match {
      case INil() => l2 match {
        case INil() => IList.nil[(Maybe[A], Maybe[B])]
        case ICons(h2, t2) => (Maybe.empty[A], Maybe.just(h2)) +: this.longZip(t2)
      }
      case ICons(h1, t1) => l2 match {
        case INil() => IList((Maybe.just(h1), Maybe.empty[B]))
        case ICons(h2, t2) => (Maybe.just(h1), Maybe.just(h2)) +: t1.longZip(t2)
      }
    }

  def zip[B](l2: IList[B]): IList[(A, B)] =
    this match {
      case INil() => IList.nil[(A, B)]
      case ICons(h1, t1) => l2 match {
        case INil() => IList.nil[(A, B)]
        case ICons(h2, t2) => (h1, h2) +: t1.zip(t2)
      }
    }
  // this.fold() {
  //   (h1, t1) => l2.fold(IList.nil[(A, B)]) {
  //     (h2, t2) => IList.cons((h1, h2), t2)
  //   }
  // }

  def +:(a: A): IList[A] = ICons(a, this)

  def ++(that: IList[A]): IList[A] =
    this match {
      case INil() => that
      case ICons(h, t) => ICons(h, t ++ that)
    }

  def reverse(): IList[A] =
    this.foldLeft(IList.nil[A])((agg, next) => IList.cons(next, agg))

  def append(that: IList[A]): IList[A] =
    // that.fold(this)(IList.cons)
    that.reverse.foldLeft(this)((agg, next) => IList.cons(next, agg))

  // def prepend(that: IList[A]): IList[A] =
  //   this.fold(that)(IList.cons)

  def map[B](f: A => B): IList[B] =
    this.reverse.foldLeft(IList.nil[B]) { (agg, next) =>
      IList.cons(f(next), agg)
    }
    // fold(IList.nil[B]) { (a, bs) =>
    //   IList.cons(f(a), bs)
    // }

}


final case class INil[A]() extends IList[A]
final case class ICons[A](head: A, tail: IList[A])  extends IList[A]


object IList {

  def apply[A](a: A*): IList[A] =
    a.foldRight(nil[A])(cons)

  def nil[A]: IList[A] = INil()

  def cons[A](head : A, tail: IList[A]): IList[A] = ICons(head, tail)

  // folds should yield identity functions when passed constructors
  def id[A](list: IList[A]): IList[A] =
    list.fold(nil[A])(cons)

  implicit def equal[A : Equal]: Equal[IList[A]] =
    // Decided not to use this... i think it breaks if one of the lists is empty (or maybe if they are different sizes)
    // new Equal[IList[A]] {
    //   def equal(l1: IList[A], l2: IList[A]) =
    //     l1.foldl2(l2)(true) {
    //       (c, a, b) => c && (a === b)
    //     }
    // }
    new Equal[IList[A]] {
      def equal(l1: IList[A], l2: IList[A]) =
        l1.longZip(l2).fold(true) {
          (a, b) => (a._1 === a._2) && b
        }
    }

  implicit def monoid[A]: Monoid[IList[A]] =
    new Monoid[IList[A]] {
      def empty =  IList.nil[A]
      def append(l1: IList[A], l2: IList[A]) = l1 ++ l2
    }

  implicit def applicative: Applicative[IList] =
    new Applicative[IList] {
      def pure[A](a: A): IList[A] = IList(a)

      // Getting a stack overflow here ...
      def ap[A, B](fa: IList[A])(fab: IList[A => B]): IList[B] =
        fa.foldLeft(IList.nil[B]) {
         (agg, next) => fab.map(f => f(next)).append(agg)
        }

      def map[A, B](i: IList[A])(f: A => B): IList[B] = i map f
    }
}
