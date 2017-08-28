package c08_testing.exercises

import c06_state.{RNG, State}

/**
  * Implement flatMap, and then use it to implement this more dynamic version of listOfN. Put flatMap and listOfN in the
  * Gen class.
  */
object Exercise06 {

  case class Gen[+A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    /* A method alias for the function we wrote earlier. */
    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, this)

    /* A version of `listOfN` that generates the size to use dynamically. */
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap (n => this.listOfN(n))

  }

  object Gen {

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  }


}
