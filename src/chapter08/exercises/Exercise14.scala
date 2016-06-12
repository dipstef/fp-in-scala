package chapter08.exercises

import chapter08.Gen
import chapter08.Gen.listOf
import chapter08.Prop._

object Exercise14 {

  def testSortedList(gen: Gen[Int]) = {
    run(forAll(listOf(gen)) { ns =>
      val nss = ns.sorted
      // We specify that every sorted list is either empty, has one element,
      // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
      nss.isEmpty || nss.tail.isEmpty ||
        (!nss.zip(nss.tail).exists { case (a, b) => a > b }
          // Also, the sorted list should have all the elements of the input list,
          && !ns.exists(!nss.contains(_))
          // and it should have no elements not in the input list.
          && !nss.exists(!ns.contains(_)))
    })
  }

  def main(args: Array[String]) {
    val smallInts = Gen.choose(-10, 10)

    testSortedList(smallInts)
  }

}
