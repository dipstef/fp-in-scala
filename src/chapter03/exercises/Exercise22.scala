package chapter03.exercises

import chapter03.{List, Cons, Nil}


/**
  * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
  *
  * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).”
  */
object Exercise22 {

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

}
