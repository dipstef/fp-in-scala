package chapter08.exercises

import chapter08.{Gen, SGen}

/**
  * Define listOf1 for generating nonempty lists, and then update your specification of max to use this generator
  *
  */
object Exercise13 {

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  def main(args: Array[String]) {
    import chapter08.Examples.testListMax

    val smallInts = Gen.choose(-10, 10)

    testListMax(smallInts)

  }
}
