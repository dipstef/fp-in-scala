package chapter08.exercises

import chapter06.{RNG, State}
import chapter08.Gen

/**
  what else we can implement using this representation of Gen. Try implementing unit, boolean, and listOfN.
  */

object Exercise05 {

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

}
