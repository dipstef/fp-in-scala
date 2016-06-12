package chapter08.exercises

import chapter06.{RNG, State}
import chapter08.Gen

object Exercise04 {

  object Gen {

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      chapter08.Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

    /* We could write this as an explicit state action, but this is far less
       convenient, since it requires us to manually thread the `RNG` through the
       computation. */
    def choose2(start: Int, stopExclusive: Int): Gen[Int] =
      chapter08.Gen(State(rng => RNG.nonNegativeInt(rng) match {
        case (n, rng2) => (start + n % (stopExclusive - start), rng2)
      }))

  }

  def main(args: Array[String]) {
    println("Gen.choose(1, 100) = " + Gen.choose(1, 100))
  }

}
