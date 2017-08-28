package chapter12.exercises

import chapter12.{Applicative, Traverse}

/**
  * Use applicative functor products to write the fusion of two traversals.
  *
  * This function will, given two functions f and g, traverse fa a single time, collecting the results of both functions at once.
  */
object Exercise18 {

  trait This[F[_]] extends Traverse[F] {

    override def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                                       (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
      traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  }

}
