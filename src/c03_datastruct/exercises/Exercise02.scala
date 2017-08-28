package c03_datastruct.exercises

import c03_datastruct.{Cons, List, Nil}

/**
  * Implement the function tail for removing the first element of a List. Note that the function takes constant time.
  *
  * What are different choices you could make in your implementation if the List is Nil?
  *
  * We’ll return to this question in the next chapter.
  */
object Exercise02 {

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

}
