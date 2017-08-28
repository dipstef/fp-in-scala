package c03_datastruct.exercises

import c03_datastruct.{Cons, List, Nil}
import c03_datastruct.List.foldRight

/**
  * Compute the length of a list using foldRight.
  *
  */
object Exercise09 {

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

}
