package chapter07.exercises

import chapter07.ParNb.{Future, Par, flatMap, map, eval}

/**
  * Implement join. Can you see how to implement flatMap using join? And can you implement join using flatMap?
  */
object Exercise14 {

  def join[A](p: Par[Par[A]]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        p(es)(
          p2 => eval(es) {
            p2(es)(cb)
          }
        )
    }

  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

}
