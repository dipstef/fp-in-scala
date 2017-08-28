package c03_datastruct.exercises

import c03_datastruct.List
import c03_datastruct.List.foldLeft

/**
  * Write sum, product, and a function to compute the length of a list using foldLeft
  */
object Exercise11 {

  def sum(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, h) => acc + 1)

}
