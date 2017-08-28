package c08_testing.exercises

import c08_testing.Gen
import c08_testing.Gen.boolean

/**
  * Implement union, for combining two generators of the same type into one, by pulling values from each generator with
  * equal likelihood.
  *
  */
object Exercise07 {

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

}
