package chapter03.exercises

import chapter03.List.foldRight
import chapter03.{List, Cons, Nil}


/**
  * Write a function that transforms a list of integers by adding 1 to each element.
  * (Reminder: this should be a pure function that returns a new List!)”
  */
object Exercise16 {

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

}
