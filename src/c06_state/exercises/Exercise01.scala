package c06_state.exercises

import c06_state.RNG
import RNG.Simple

/**
  *Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
  *Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative
  *counterpart.
  */

object Exercise01 {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    // `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
    (if (i < 0) -(i + 1) else i, r)
  }


  def main(args: Array[String]) {
    val rng = new Simple(123)
    println("nonNegativeInt(r) = " + nonNegativeInt(rng))

    assert(nonNegativeInt(rng) == nonNegativeInt(rng))
  }
}
