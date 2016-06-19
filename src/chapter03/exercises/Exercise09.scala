package chapter03.exercises

import chapter03.{Cons, List, Nil}
import chapter03.List.foldRight

/**
  * Compute the length of a list using foldRight.
  *
  */
object Exercise09 {

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

}
