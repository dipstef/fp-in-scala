package c12_applicative.exercises

import c12_applicative.{Applicative, Traverse}

object Exercise19 {

  trait This[F[_]] extends Traverse[F] {
    self =>

    override def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
      new Traverse[({type f[x] = F[G[x]]})#f] {
        override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
          self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
      }

  }
}
