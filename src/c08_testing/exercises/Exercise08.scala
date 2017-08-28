package c08_testing.exercises

import c06_state.{RNG, State}
import c08_testing.Gen

/**
  * Implement weighted, a version of union that accepts a weight for each Gen and generates values from each Gen with
  * probability proportional to its weight.
  */
object Exercise08 {

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }
}
