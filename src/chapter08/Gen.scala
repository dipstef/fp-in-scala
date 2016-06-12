package chapter08

import chapter06.{RNG, State}
import chapter08.exercises.Exercise04

case class Gen[+A](sample: State[RNG,A]) {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Exercise04.choose(start,stopExclusive)
}
