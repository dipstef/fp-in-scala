package c08_testing.exercises

import c08_testing.{Gen, SGen}

/**
  **
 Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen instead of a Gen.
  *The implementation should generate lists of the requested size.
 *
 */
object Exercise12 {

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))
}
