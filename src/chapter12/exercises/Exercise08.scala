package chapter12.exercises

import chapter12.Applicative

/**
  * Just like we can take the product of two monoids A and B to give the monoid (A, B), we can take the product of two
  * applicative functors. Implement this function:
  */
object Exercise08 {

  trait This[F[_]] extends Applicative[F]{

    override def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
      val self = this

      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        // compiles even if highlighted as error from IntelliJ
        override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
          (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
      }
    }


  }

}
