package chapter06

import chapter06.exercises._


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

class ARNG {
  type Rand[+A] = RNG => (A, RNG)

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
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = Exercise01.nonNegativeInt(rng)

  def double(rng: RNG): (Double, RNG) = Exercise02.double(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = Exercise03.intDouble(rng)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = Exercise03.doubleInt(rng)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = Exercise03.double3(rng)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = Exercise04.ints(count)(rng)


  // The Form (A, RNG) is called state action. These state actions can be combined using combinators.
  // We want to write combinators that let us combine Rand actions while avoiding explicitly passing along the RNG state.
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // a simple RNG state transition is the unit action, which passes the RNG state through without using it,
  // always returning a constant value rather than a random value”
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // transforming the output of a state action without modifying the state itself.
  // Rand[A] is  a type alias for a function type RNG => (A, RNG), so this is just a kind of function composition
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Example on how to use map
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5
  def double = Exercise05.double

  /* Unfortunately, map isn’t powerful enough to implement intDouble and doubleInt from exercise 3.
     What we need is a new combinator map2 that can combine two RNG actions into one using a binary rather than unary
     function */

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = Exercise06.map2(ra, rb)(f)

}
