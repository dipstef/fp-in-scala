package c05_laziness.exercises


/**
  * Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the REPL.
  *
  * You can convert to the regular List type in the standard library.
  *
  * You can place this and other functions that operate on a Stream inside the Stream trait.
  */
object Exercise01 {

  trait Stream[+A] {

    // The natural recursive solution
    def toListRecursive: List[A] = this match {
      case Cons(h, t) => h() :: t().toListRecursive
      case _ => List()
    }

    /* The above solution will stack overflow for large streams, since it's not tail-recursive. Here is a tail-recursive
       implementation.
       At each step we cons onto the front of the `acc` list, which will result in the reverse of the stream.
       Then at the end we reverse the result to get the correct order again.
    */
    def toList: List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }
      go(this, List()).reverse
    }

    /*  In order to avoid the `reverse` at the end, we could write it using a mutable list buffer and an explicit loop
        instead.
        Note that the mutable list buffer never escapes our `toList` method, so this function is still _pure_.
    */
    def toListFast: List[A] = {
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

  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

}