package com.c12e.learn
package data


sealed trait \/[A, B]

final case class -\/[A, B](a: A) extends (A \/ B)
final case class \/-[A, B](b: B) extends (A \/ B)


object \/ {

  def left[A, B](a: A): A \/ B = -\/(a)
  def right[A, B](b: B): A \/ B = \/-(b)

}
