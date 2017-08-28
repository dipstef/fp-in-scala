package c05_laziness.exercises

/**
  * Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the intermediate
  * results.
  *
  * For example:
  *
  * scala> Stream(1,2,3).scanRight(0)(_ + _).toList
  *   res0: List[Int] = List(6,5,3,0)
  *
  * This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0).
  *
  * Your function should reuse intermediate results so that traversing a Stream with n elements always takes time linear
  * in n.
  *
  * Can it be implemented using unfold? How, or why not? Could it be implemented using another function we’ve written?
  *
  */
object Exercise16 {

  import Stream._

  trait Stream[+A] {

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a, p0) => {
        // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
      })._2


    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((h, s)) => cons(h, unfold(s)(f))
        case None => empty
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

  }

}
