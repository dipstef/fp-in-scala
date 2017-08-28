package chapter12.exercises

import chapter12.Applicative

/**
  * Transplant the implementations of as many combinators as you can from Monad to Applicative, using only map2 and unit,
  * or methods implemented in terms of them.
  */
object Exercise01 {

  trait This[F[_]] extends Applicative[F] {

    override def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

    override def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
  }

}
