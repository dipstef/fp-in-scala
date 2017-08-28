package c05_laziness.exercises



/**
  * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
  *
  * The zipAll function should continue the traversal as long as either stream has more elements—it uses Option
  * to indicate whether each stream has been exhausted
  */
object Exercise13 {

  import Stream._

  trait Stream[+A] {

    def mapd[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }

    def take(n: Int): Stream[A] =
      unfold((this, n)) {
        case (Cons(h, t), 1) => Some((h(), (empty, 0)))
        case (Cons(h, t), n_) if n > 1 => Some((h(), (t(), n_ - 1)))
        case _ => None
      }

    def take(f: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if f(h()) => Some((h(), t()))
        case _ => None
      }

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }

    // special case of `zipWith`
    def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))


    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s2)((_, _))

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      Stream.unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
        case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
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
