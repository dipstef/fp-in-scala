package c08_testing.exercises

import c06_state.{RNG, State}
import c08_testing.Gen

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
