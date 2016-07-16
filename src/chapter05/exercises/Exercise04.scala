package chapter05.exercises

/**
  * Implement forAll, which checks that all elements in the Stream match a given predicate.
  *
  * Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
  */
object Exercise04 {


  trait Stream[+A] {

    // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name
    // and may choose not to evaluate it.
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }


    // Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element
    // is found.
    def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


}
