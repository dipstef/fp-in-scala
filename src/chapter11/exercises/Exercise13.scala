package chapter11.exercises

import chapter11.Monad

/**
  * Implement either flatMap or compose in terms of join and map.
  */
object Exercise13 {

  trait This[M[_]] extends Monad[M] {

    override def __flatMap[A, B](ma: M[A])(f: (A) => M[B]): M[B] = join(map(ma)(f))

    override def __compose[A, B, C](f: (A) => M[B], g: (B) => M[C]): (A) => M[C] = a => join(map(f(a))(g))

  }

}