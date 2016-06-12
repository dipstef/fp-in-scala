package chapter06.exercises

import chapter06.RNG
import chapter06.exercises.Exercise01.nonNegativeInt
import RNG.Simple

/**
  * Write a function to generate a Double between 0 and 1, not including 1. Note: You can use Int.MaxValue to obtain the
  * maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.
  */
object Exercise02 {

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Integer.MAX_VALUE.toDouble + 1), r)
  }

  def main(args: Array[String]) {
    val rng = new Simple(123)

    println("Integer.MAX_VALUE = " + Integer.MAX_VALUE)

    println("Integer.MAX_VALUE + 1 = " + (Integer.MAX_VALUE + 1))

    println("Integer.MAX_VALUE.toDouble + 1 = " + (Integer.MAX_VALUE.toDouble + 1))

    println("Integer.MAX_VALUE / (Integer.MAX_VALUE.toDouble + 1)  = " +
      Integer.MAX_VALUE / (Integer.MAX_VALUE.toDouble + 1))

    println("double(rng) = " + double(rng))
  }
}
