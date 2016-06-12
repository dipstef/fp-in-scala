package chapter06.exercises

import chapter06.RNG
import chapter06.RNG.Simple

/**
  * Write a function to generate a list of random integers.”
  */
object Exercise04 {

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = intsTailRecursive(count)(rng)

  def intsRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case x if x <= 0 => (List(), rng)
      case _ =>
        val (x, r1) = rng.nextInt
        val (xs, r2) = intsRecursive(count - 1)(r1)
        (x :: xs, r2)
    }
  }

  def intsTailRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      count match {
        case x if x <= 0 => (xs, r)
        case _ =>
          val (x, r2) = r.nextInt
          go(count -1, r2, x::xs)
      }
    go(count, rng, List())
  }

  def main(args: Array[String]) {
    val rng = Simple(123)

    // elements are the same but in different order
    println("intsRecursive(count=3)(rng) = " + intsRecursive(count=3)(rng))
    println("intsTailRecursive(count=3)(rng) = " + intsTailRecursive(count=3)(rng))
  }

}
