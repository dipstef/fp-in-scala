package chapter08.exercises

import chapter07.Par
import chapter07.Par.Par
import chapter08.Gen
import chapter08.Gen._

/**
  Write a richer generator for Par[Int], which builds more deeply nested parallel computations than the simple ones
  we gave previously.
  */
class Exercise16 {
  /* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
 * computation for each element of the input list summed to produce the final
 * result. This is not the most compelling example, but it provides at least some
 * variation in structure to use for testing.
 */
  val pint2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }))
}
