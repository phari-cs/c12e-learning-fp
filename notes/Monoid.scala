package com.c12e.learn

// Guy Steele - wonderful talk on "Growing a Language" - wrote a language called scheme - dialect of lisp
//  In his talk, wanted to take commuitivity, associativity, identiity, zero, ...
//   Just need to know what doesnt matter
//    associativity - means grouping doesnt matter
//    commuitivity - means the order doesnt matter
//      plus is both associative, and commuitive
//    identiity - I dont matter ... the identity doesnt matter (1 * ...) - the 1 doenst matter
//    zero - Nothing else matters ..... (0 * ...) doesnt matter what anything else is ...

// Monoid has everything that semigroup has plus some more
// Monoid is a zero

// final case class Max[A](a: A) extends AnyVal

trait Monoid[A] {
    def empty: A
    // must be associative a append b append c = ((a app b) append) = (a append (b append c))
    def append(a1: A, a2: A): A
}

trait MonoidInstances {

  implicit val stringMonoid: Monoid[String] =
      new Monoid[String] {
          def append(a1: String, a2: String) = a1 + a2
          def empty = ""
      }
  implicit val maxMonoid: Monoid[Max[Int]] =
      new Monoid[Max[Int]] {
          // append here actually means find max
          def append(a1: Max[Int], a2: Max[Int]) = Max(math.max(a1.a, a2.a))
          // Always get the other int back ...
          // Max is a type, MaxValue is a value
          // Need to box value in Max, only understands Max[Ints] not Ints
          def empty = Max(Int.MinValue)
      }

  implicit def tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] = {
      new Monoid[(A, B)] {
          // Since apply is implicit, Monoid[A] goes and pulls that semigroup...
          def append(p1: (A, B), p2: (A, B)): (A, B) = (Monoid[A].append(p1._1, p2._1), Monoid[B].append(p1._2, p2._2))
          def empty = (Monoid[A].empty, Monoid[B].empty)
      }
  }

}

object Monoid extends MonoidInstances{
    // performs optimization
    // makes a call go away...
    // most of the time compiler notices whats inlineable,
    // here, it just happens that inlining is obvious
    // only really ever put it here...
    // early people aware of compiler limitations,
    // now focus less on compiler limitaitons..
    @inline def apply[A](implicit ev: Monoid[A]) = ev
}

final class MonoidOps[A](val a: A) extends AnyVal {
    def |+|(a2: A)(implicit ev1: Monoid[A]) = ev1.append(a, a2)
}

trait MonoidSyntax {
    implicit def monoidToOps[A: Monoid](a:A): MonoidOps[A] =
        new MonoidOps(a)

    // An empty constructor ... dont have to type <Monoid>.empty, can just type empty and it will implicitly find the empty of the monoid
    def empty[A](implicit ev: Monoid[A]) = ev.empty
}

object MonoidSyntax extends MonoidSyntax

object ExampleApp2 extends App {
    println(implicitly[Monoid[String]].append("a", "b"))
    println(Monoid[String].append("a", "b"))
    println(Monoid.apply(Monoid.stringMonoid).append("a", "b"))
}
