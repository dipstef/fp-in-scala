package c04_error.exercises

import c04_error.{Option, Some}
import c04_error.Option.map2

/**
  * Write a function sequence that combines a list of Options into one Option containing a list of all the Some values
  * in the original list.
  *
  * If the original list contains None even once, the result of the function should be None; otherwise the result should
  * be Some with a list of all the values.
  */
object Exercise04 {

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

}
