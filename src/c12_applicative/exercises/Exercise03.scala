package c12_applicative.exercises

import c12_applicative.Applicative

/**
  * The apply method is useful for implementing map3, map4, and so on, and the pattern is straightforward.
  *
  * Implement map3 and map4 using only unit, apply, and the curried method available on functions.[1]
  *
  * 1 Recall that given f: (A,B) => C, f.curried has type A => B => C. A curried method exists for functions of any arity in Scala.
  */
object Exercise03 {

  trait This[F[_]] extends Applicative[F] {


    override def map3[A, B, C, D](fa: F[A],
                                  fb: F[B],
                                  fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    override def map4[A, B, C, D, E](fa: F[A],
                                     fb: F[B],
                                     fc: F[C],
                                     fd: F[D])(f: (A, B, C, D) => E): F[E] =

      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }


}
