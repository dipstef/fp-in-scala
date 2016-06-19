package chapter03.exercises

import chapter03.{Cons, List}
import chapter03.List.foldLeft


/**
  * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
  * See if you can write it using a fold.
  */
object Exercise12 {


  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

}
