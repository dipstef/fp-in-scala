package chapter05.exercises

/**
  * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
  */
object Exercise03 {

  import Stream._

  trait Stream[+A] {

    // It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
      case _ => empty
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

  }


}
