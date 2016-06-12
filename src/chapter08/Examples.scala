package chapter08

import chapter08.Gen.listOf
import chapter08.Prop.{forAll, run}

object Examples {

  def main(args: Array[String]) {
    val smallInts = Gen.choose(-10, 10)

    run(forAll(listOf(smallInts)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    })
  }
}
