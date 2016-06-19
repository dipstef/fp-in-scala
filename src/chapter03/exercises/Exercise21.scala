package chapter03.exercises

import chapter03.List.flatMap
import chapter03.{List, Nil}


/**
  * Use flatMap to implement filter
  */
object Exercise21 {

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

}
