package chapter10.exercises

import chapter10.Monoid

/**
  * Give a Monoid instance for combining Option values.
  */
object Exercise02 {

  // Notice that we have a choice in how we implement `op`.

  // We can compose the options in either order. Both of those implementations satisfy the monoid laws, but they are
  // not equivalent.
  // This is true in general--that is, every monoid has a _dual_ where the `op` combines things in the opposite order.
  // Monoids like `booleanOr` and `intAddition` are equivalent to their duals because their `op` is commutative
  // as well as associative.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]): Option[A] = x orElse y

    val zero: Option[A] = None
  }

}
