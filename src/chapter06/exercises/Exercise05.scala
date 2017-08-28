package chapter06.exercises

import chapter06.RNG.{Rand, Simple, map, nonNegativeInt}

/**
  * Use map to reimplement double in a more elegant way
  */
object Exercise05 {

  def double: Rand[Double] = map(nonNegativeInt)(i => i / (Integer.MAX_VALUE.toDouble + 1))


  def main(args: Array[String]) {
    val rng = Simple(123)

    println("rng.double = " + double(rng))
  }
}
