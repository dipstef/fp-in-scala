package chapter08

import chapter08.exercises.Exercise13.testListMax
import chapter08.exercises.Exercise14.testSortedList

object Examples {

  def main(args: Array[String]) {
    val smallInts = Gen.choose(-10, 10)

    testListMax(smallInts)
    testSortedList(smallInts)

  }
}
