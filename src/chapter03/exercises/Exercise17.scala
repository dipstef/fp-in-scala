package chapter03.exercises

import chapter03.{Cons, List, Nil}
import chapter03.List._

/**
  * Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString
  * to convert some d: Double to a String.
  */
object Exercise17 {

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))


}
