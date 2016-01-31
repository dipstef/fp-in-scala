package chapter06

import chapter06.exercises.{Exercise03, Exercise01, Exercise02}


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}


object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) = Exercise01.nonNegativeInt(rng)

    def double(rng: RNG): (Double, RNG) = Exercise02.double(rng)

    def intDouble(rng: RNG): ((Int,Double), RNG) = Exercise03.intDouble(rng)

    def doubleInt(rng: RNG): ((Double,Int), RNG) = Exercise03.doubleInt(rng)

    def double3(rng: RNG): ((Double,Double,Double), RNG) = Exercise03.double3(rng)


  }
}
