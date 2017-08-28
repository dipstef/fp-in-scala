package c03_datastruct.exercises

import c03_datastruct.{Cons, List, Nil}

/**
  * Using the same idea, implement the function setHead for replacing the first element of a List with a different value
  */
object Exercise03 {

  def setHead[A](l: List[A], head: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(head, xs)
  }

}
