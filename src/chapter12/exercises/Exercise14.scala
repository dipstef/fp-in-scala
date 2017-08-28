package chapter12.exercises

import chapter12.{Applicative, Monad, Traverse}

/**
  * Hard: Implement map in terms of traverse as a method on Traverse[F]. This establishes that Traverse is an extension
  * of Functor and that the traverse function is a generalization of map (for this reason we sometimes call these traversable functors).
  *
  * Note that in implementing map, you can call traverse with your choice of Applicative[G].
  */
object Exercise14 {

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): A = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  trait This[F[_]] extends Traverse[F] {

    override def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] = super.sequence(fma)
    override def traverse[G[_] : Applicative, A, B](fa: F[A])(f: (A) => G[B]): G[F[B]] = super.traverse(fa)(f)

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      traverse[Id, A, B](fa)(f)(idMonad)

    // Other way to define implicits

    def map_[A, B](fa: F[A])(f: A => B): F[B] =
      traverse_[Id, A, B](fa)(f)(idMonad)

    def traverse_[G[_],A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
      sequence_(map_(fa)(f))

    def sequence_[G[_],A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
      traverse_(fga)(ga => ga)

  }

}
