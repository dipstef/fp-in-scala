package c05_laziness.exercises


/**
  * Write the function take(n) for returning the first n elements of a Stream,
  * and drop(n) for skipping the first n elements of a Stream.
  */
object Exercise02 {

  import Stream._

  trait Stream[+A] {

    /* Create a new Stream[A] from taking the n first elements from this.

       We can achieve that by recursively calling take on the invoked tail of a cons cell.
       We make sure that the tail is not invoked unless we need to, by handling the special case where n == 1 separately.

       If n == 0, we can avoid looking at the stream at all.
     */
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    /* Create a new Stream[A] from this, but ignore the n first elements. This can be achieved by recursively calling
       drop on the invoked tail of a cons cell. Note that the implementation is also tail recursive.
     */
    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  }

  case object Empty extends Stream[Nothing]

  // looks identical to our List type, except that the Cons data constructor takes explicit thunks (() => A
  // and () => Stream[A]) instead of regular strict values
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    /* smart constructors, which is what we call a function for constructing a data type that ensures some additional
       invariant or provides a slightly different signature than the “real” constructors used for pattern matchin

       our cons smart constructor takes care of memoizing the by-name arguments for the head and tail of the Cons.
       This is a common trick, and it ensures that our thunk will only do its work once, when forced for the first time.
       Subsequent forces will return the cached lazy val
     */
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

  }


}
