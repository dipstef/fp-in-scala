package chapter03.exercises
import chapter03.List.foldRight
import chapter03.{Cons, List}

/**
  * Implement append in terms of either foldLeft or foldRight
  */
object Exercise14 {

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

}
