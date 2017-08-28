package c10_monoids.exercises

import c10_monoids.Monoid

/**
  * Implement foldMap.
  */
object Exercise05 {

  // Notice that this function does not require the use of `map` at all.
  // All we need is `foldLeft`.
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero){(b, a) => m.op(b, f(a))}

}
