package com.c12e.learn


sealed abstract class Nel[A] { self =>
  def head: A
  def tail: IList[A]

  def ++(that: Nel[A]): Nel[A] = {
    new Nel[A] {
      def head = self.head
      def tail = self.tail ++ (that.head +: that.tail)
    }
  }
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
}
