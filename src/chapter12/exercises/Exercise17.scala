package chapter12.exercises

import chapter12.Traverse

/**
  * Use mapAccum to give a default implementation of foldLeft for the Traverse trait.
  */
object Exercise17 {

  // This implementation is very similar to `toList` except instead of accumulating into a list, we are accumulating
  // into a `B` using the `f` function.
  trait This[F[_]] extends Traverse[F] {

    override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
      mapAccum(fa, z)((a, b) => ((), f(b, a)))._2
  }

}
