package c05_laziness.exercises

import c05_laziness.Stream.unfold

/**
  * Write fibs, from, constant, and ones in terms of unfold
  */
object Exercise12 {

  val fibs =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def from(n: Int) =
    unfold(n)(n => Some((n, n + 1)))

  def constant[A](a: A) =
    unfold(a)(_ => Some((a, a)))

  // could also of course be implemented as constant(1)
  val ones = unfold(1)(_ => Some((1, 1)))

}
