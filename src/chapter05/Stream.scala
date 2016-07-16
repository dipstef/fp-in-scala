package chapter05


import Stream._

trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name
  // and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
  this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
  // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`,
  // `b` will never be evaluated and the computation terminates early.
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }


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


  // It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }


  // Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element
  // is found.
  def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)


  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
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

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
