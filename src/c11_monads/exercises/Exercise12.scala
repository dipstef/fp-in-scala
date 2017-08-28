package c11_monads.exercises

import c11_monads.Monad

/**
  * There’s a third minimal set of monadic combinators: map, unit, and join. Implement join in terms of flatMap.
  */
object Exercise12 {

  trait This[M[_]] extends Monad[M] {
    override def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)
  }

  def main(args: Array[String]): Unit = {

    val listMonad = new This[List] {
      override def unit[A](a: => A): List[A] = List(a)
      override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)
    }

    println(s"result = ${listMonad.join(List(List(1,2,3), List(4, 5, 6)))}")
  }
}
