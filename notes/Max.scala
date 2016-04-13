package com.c12e.learn


// "New typing"

// AnyVal - makes it so that its not a performance hit
// Dont box - Value Classes / New Types
// Extending case classes is a defect - compiler forces you to make it final

final case class Max[A](a: A) extends AnyVal
// case classes makes setters and getter, constructors
