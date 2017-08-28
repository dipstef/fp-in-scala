package c12_applicative.exercises

import c12_applicative.Traverse

/**
  * There’s an interesting consequence of being able to turn any traversable functor into a reversed list—we can write,
  * once and for all, a function to reverse any traversable functor! Write this function, and think about what it means
  * for List, Tree, and other traversable functors.
  *
  * def reverse[A](fa: F[A]): F[A]
  *
  * It should obey the following law, for all x and y of the appropriate types:
  *
  * toList(reverse(x)) ++ toList(reverse(y)) == reverse(toList(y) ++ toList(x))
  */
object Exercise16 {

  trait This[F[_]] extends Traverse[F] {

    override def reverse[A](fa: F[A]): F[A] =
      mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  }


}
