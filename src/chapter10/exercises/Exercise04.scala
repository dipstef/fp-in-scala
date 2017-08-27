package chapter10.exercises

import chapter08.{Gen, Prop}
import chapter08.Prop.forAll
import chapter10.Monoid

/**
  * Use the property-based testing framework we developed in part 2 to implement a property for the monoid laws.
  * Use your property to test the monoids we’ve written.
  *
  * def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop
  */
object Exercise04 {

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  // Associativity
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      // Identity
      forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(m.zero, a) == a)



}
