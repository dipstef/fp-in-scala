package chapter08.exaustive

import java.util.concurrent.{ExecutorService, Executors}

import chapter07.Par
import chapter07.Par._
import chapter08.exaustive.Gen._
import chapter08.exaustive.Prop._
import chapter08.exaustive.PropParallel._

import scala.language.{implicitConversions, postfixOps}

object Example {

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)


  val p2 = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  val p3 = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES) get
  }

  val pint = Gen.choose(0, 10) map Par.unit
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  /* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
 * computation for each element of the input list summed to produce the final
 * result. This is not the most compelling example, but it provides at least some
 * variation in structure to use for testing.
 */
  lazy val pint2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }))


  val forkProp = forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"

}
