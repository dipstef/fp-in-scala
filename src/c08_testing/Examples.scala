package c08_testing

import c08_testing.exercises.Exercise13.testListMax
import c08_testing.exercises.Exercise14.testSortedList

object Examples {

  def main(args: Array[String]) {
    val smallInts = Gen.choose(-10, 10)

    testListMax(smallInts)
    testSortedList(smallInts)

  }
}
