package chapter05.exercises

object Exercise06 {


  trait Stream[+A] {

    // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name
    // and may choose not to evaluate it.
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }


    def headOption: Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

  }


  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


}
