package c11_monads.exercises

import c11_monads.Monad

/**
  * One combinator we saw for Gen and Parser was listOfN, which allowed us to replicate a parser or generator n times
  * to get a parser or generator of lists of that length.
  *
  * We can implement this combinator for all monads F by adding it to our Monad trait. We should also give it a more
  * generic name such as replicateM
  */
object Exercise04 {

  // For `List`, the `replicateM` function will generate a list of lists. It will contain all the lists of length `n`
  // with elements selected from the input list.

  // For `Option`, it will generate either `Some` or `None` based on whether the input is `Some` or `None`.
  // The `Some` case will contain a list of length `n` that repeats the element in the input `Option`.

  // The general meaning of `replicateM` is described very well by the implementation `sequence(List.fill(n)(ma))`.
  //
  // It repeats the `ma` monadic value `n` times and gathers the results in a single value, where the monad `M`
  // determines how values are actually combined.

  trait This[M[_]] extends Monad[M] {

    override def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

    // Recursive version:
    def _replicateM[A](n: Int, ma: M[A]): M[List[A]] =
      if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)


  }

}