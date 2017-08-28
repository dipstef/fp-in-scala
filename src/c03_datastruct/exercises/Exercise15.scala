package c03_datastruct.exercises

import c03_datastruct.List.{append, foldRight}
import c03_datastruct.{List, Nil}

/**
  * Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the total
  * length of all lists. Try to use functions we have already defined.
  */
object Exercise15 {

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

}
