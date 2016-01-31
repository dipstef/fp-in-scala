package chapter06.exercises

import chapter06.RNG.{Rand, SimpleRNG, map, nonNegativeInt}

/**
  * Use map to reimplement double in a more elegant way
  */
object Exercise05 {


  // Exercise 5
  def double: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Integer.MAX_VALUE.toDouble + 1))
  }

  def main(args: Array[String]) {
    val rng = new SimpleRNG(123)

    println("rng.double = " + double(rng))
  }
}