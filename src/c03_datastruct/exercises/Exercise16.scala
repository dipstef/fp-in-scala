package c03_datastruct.exercises

import c03_datastruct.List.foldRight
import c03_datastruct.{List, Cons, Nil}


/**
  * Write a function that transforms a list of integers by adding 1 to each element.
  * (Reminder: this should be a pure function that returns a new List!)”
  */
object Exercise16 {

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

}
