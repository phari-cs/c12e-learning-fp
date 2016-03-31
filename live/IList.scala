package com.c12e.learn


sealed abstract class IList[A] {

  def fold[B](ifNil: B)(ifCons: (A, B) => B): B =
    this match {
      case INil() => ifNil
      case ICons(h, t) => ifCons(h, t.fold(ifNil)(ifCons))
    }

  def ++(that: IList[A]): IList[A] =
    this match {
      case INil() => that
      case ICons(h, t) => ICons(h, t ++ that)
    }

}

final case class INil[A]() extends IList[A]
final case class ICons[A](head: A, tail: IList[A])  extends IList[A]


object IList {

   def nil[A]: IList[A] = INil()
   def cons[A](head : A, tail: IList[A]): IList[A] = ICons(head, tail)

   // folds should yield identity functions when passed constructors
   def id[A](list: IList[A]): IList[A] =
     list.fold(nil[A])(cons)

   implicit def ilistSemigroup[A]: Semigroup[IList[A]] =
     new Semigroup[IList[A]] {
       def append(l1: IList[A], l2: IList[A]) = l1 ++ l2
     }

}
