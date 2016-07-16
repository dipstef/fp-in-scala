package chapter05.exercises

import chapter05.exercises.Exercise05.Cons
import chapter05.exercises.Exercise06.Cons

/**
  * Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.
  */
object Exercise07 {

  import Stream._

  trait Stream[+A] {

    // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name
    // and may choose not to evaluate it.
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }


    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) =>
        if (f(h)) cons(h, t)
        else t)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h, t) => f(h) append t)


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
