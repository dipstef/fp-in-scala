package c03_datastruct.exercises

import c03_datastruct.{Cons, List}
import c03_datastruct.List.foldLeft


/**
  * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
  * See if you can write it using a fold.
  */
object Exercise12 {

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]()){case (acc, x) => Cons(x, acc)}

}
