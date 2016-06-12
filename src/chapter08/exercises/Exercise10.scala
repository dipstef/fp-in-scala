package chapter08.exercises

import chapter06.{RNG, State}

/**
  * Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
  *
  */
object Exercise10 {

  case class Gen[+A](sample: State[RNG, A]) {
    def unsized = SGen(_ => this)
  }

  case class SGen[+A](forSize: Int => Gen[A]) {

  }


}
