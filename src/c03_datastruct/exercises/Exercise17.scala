package c03_datastruct.exercises

import c03_datastruct.{Cons, List, Nil}
import c03_datastruct.List._

/**
  * Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString
  * to convert some d: Double to a String.
  */
object Exercise17 {

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))


}
