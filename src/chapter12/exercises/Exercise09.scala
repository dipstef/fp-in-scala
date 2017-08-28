package chapter12.exercises

import chapter12.Applicative

/**
  * Hard: Applicative functors also compose another way! If F[_] and G[_] are applicative functors, then so is F[G[_]].
  * Implement this function:
  */
object Exercise09 {

  trait This[F[_]] extends Applicative[F] {

    override def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
      val self = this
      new Applicative[({type f[x] = F[G[x]]})#f] {
        def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C): F[G[C]] =
          self.map2(fga, fgb)(G.map2(_,_)(f))
      }
    }

  }


}
