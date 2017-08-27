package chapter11.exercises

import chapter11.Monad


/**
  * Implement the Kleisli composition function compose
  */
object Exercise07 {

  trait This[M[_]] extends Monad[M] {
    override def compose[A, B, C](f: (A) => M[B], g: (B) => M[C]): (A) => M[C] =
      a => flatMap(f(a))(g)
  }

}