package chapter04.exercises

import chapter04.{Option, Some}
import chapter04.Option.map2


/**
  * Sometimes we’ll want to map over a list using a function that might fail, returning None if applying it to any
  * element of the list returns None
  *
  * Implement this function. It’s straightforward to do using map and sequence, but try for a more efficient
  * implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
  */
object Exercise05 {

  //can be written with explicit recursion or use `foldRight` to do the recursion for you.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
