package c08_testing.exercises

import c08_testing.Gen.{apply => _, _}
import c08_testing.Prop._
import c08_testing.{Gen, SGen}

/**
  * Define listOf1 for generating nonempty lists, and then update your specification of max to use this generator
  *
  */
object Exercise13 {

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  def testListMax(gen: Gen[Int]): Unit = {
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
