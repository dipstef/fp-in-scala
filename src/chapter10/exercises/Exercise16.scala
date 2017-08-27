package chapter10.exercises

import chapter10.Monoid

/**
  * Prove it. Notice that your implementation of op is obviously associative so long as A.op and B.op are both associative.
  *
  */
object Exercise16 {

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) =
        (A.op(x._1, y._1), B.op(x._2, y._2))

      val zero: (A, B) = (A.zero, B.zero)
    }

}
