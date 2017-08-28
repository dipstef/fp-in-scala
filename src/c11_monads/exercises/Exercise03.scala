package c11_monads.exercises

import c11_monads.Monad

/**
  * “The sequence and traverse combinators should be pretty familiar to you by now, and your implementations of them
  * from various prior chapters are probably all very similar. Implement them once and for all on Monad[F].
  */
object Exercise03 {

  trait This[M[_]] extends Monad[M] {

    override def sequence[A](lma: List[M[A]]): M[List[A]] = {
      lma.foldRight(unit(List[A]())){(ma, acc) => map2(ma, acc)(_ :: _)}
    }

    override def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = {
      la.foldRight(unit(List[B]())){ (a, acc) => map2(f(a), acc)(_ :: _ )}
    }


  }

}
