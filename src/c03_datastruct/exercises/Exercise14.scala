package c03_datastruct.exercises
import c03_datastruct.List.foldRight
import c03_datastruct.{Cons, List}

/**
  * Implement append in terms of either foldLeft or foldRight
  */
object Exercise14 {

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

}
