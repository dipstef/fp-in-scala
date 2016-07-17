package chapter05.exercises

import chapter05.{Stream, Cons}

/**
  * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
  */
object Exercise08 {

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

}
