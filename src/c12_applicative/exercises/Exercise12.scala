package c12_applicative.exercises

import c12_applicative.Applicative

/**
  * On the Applicative trait, implement sequence over a Map rather than a List:
  */
object Exercise12 {

  trait This[F[_]] extends Applicative[F] {
    override def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
      (ofa foldLeft unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
        map2(acc, fv)((m, v) => m + (k -> v))
      }
  }

}
