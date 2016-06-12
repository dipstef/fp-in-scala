package chapter06.exercises

import chapter06.RNG
import chapter06.RNG.Simple
import chapter06.exercises.Exercise02.double

/**
  * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
  * You should be able to reuse the functions you’ve already written.”
  */
object Exercise03 {

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def main(args: Array[String]) {
    val rng: Simple = Simple(123)

    println("intDouble(rng) = " + intDouble(rng))
    println("doubleInt(rng) = " + doubleInt(rng))
    println("double3(rng) = " + double3(rng))
  }

}
