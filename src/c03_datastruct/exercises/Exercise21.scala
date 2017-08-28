package c03_datastruct.exercises

import c03_datastruct.List.flatMap
import c03_datastruct.{List, Nil}


/**
  * Use flatMap to implement filter
  */
object Exercise21 {

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

}
