package chapter05.exercises


/**
  * Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
  *
  * For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
  */
object Exercise14 {


  import Stream._

  trait Stream[+A] {

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s2)((_, _))

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      Stream.unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
        case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      }

    def takeWhile(f: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if f(h()) => Some((h(), t()))
        case _ => None
      }

    def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }


    def startsWith[B](s: Stream[B]): Boolean =
      zipAll(s).takeWhile(_._2.isDefined) forAll {
        case (h, h2) => h == h2
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
        case Some((h,s)) => cons(h, unfold(s)(f))
        case None => empty
      }

  }

}
