package chapter08

import chapter08.Gen.listOf1
import chapter08.Prop.{forAll, run}

object Examples {

  def testListMax(gen: Gen[Int]) = {
    run(forAll(listOf1(gen)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    })
  }

  def main(args: Array[String]) {
    val smallInts = Gen.choose(-10, 10)

    testListMax(smallInts)

  }
}
