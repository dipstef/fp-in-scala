package chapter03.exercises

import chapter03.{Cons, List, Nil}

/**
  * Generalize the function you just wrote so that it’s not specific to integers or addition. Name your generalized
  * function zipWith.
  */
object Exercise23 {

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))

  }
}