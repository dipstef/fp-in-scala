package chapter10.exercises

import chapter10.Monoid

/**
  * Write a monoid instance for functions whose results are monoids.
  */
object Exercise17 {

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f: A => B, g: A => B): (A) => B = a => B.op(f(a), g(a))

      val zero: A => B = _ => B.zero
    }


}
