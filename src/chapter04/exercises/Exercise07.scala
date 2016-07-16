package chapter04.exercises

import chapter04.{Either, Right}

/**
  * Implement sequence and traverse for Either.
  *
  * These should return the first error that’s encountered, if there is one.
  */
object Exercise07 {

  //you can pattern-match the list and use explicit recursion or use `foldRight` to perform the recursion for you.
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f)) (_ :: _)
    }

  def traverse_1[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)
}
