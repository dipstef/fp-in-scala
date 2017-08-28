package c08_testing.exercises

import c07_parallelism.Par
import c07_parallelism.Par.Par
import c08_testing.Gen
import c08_testing.Gen._

/**
  Write a richer generator for Par[Int], which builds more deeply nested parallel computations than the simple ones
  we gave previously.
  */
object Exercise16 {
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
