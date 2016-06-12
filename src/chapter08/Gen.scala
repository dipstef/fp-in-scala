package chapter08

import chapter06.{RNG, State}
import chapter08.exercises.{Exercise04, Exercise05}

case class Gen[+A](sample: State[RNG, A]) {

}

object Gen {
  def unit[A](a: => A): Gen[A] = Exercise05.unit(a)

  def boolean: Gen[Boolean] = Exercise05.boolean

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Exercise04.choose(start, stopExclusive)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Exercise05.listOfN(n, g)

}
