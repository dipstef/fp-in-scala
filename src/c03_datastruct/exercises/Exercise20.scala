package c03_datastruct.exercises

import c03_datastruct.List.{append, concat, foldRight, map}
import c03_datastruct.{List, Nil}


/**
  * Write a function flatMap that works like map except that the function given will return a list instead of a single
  * result, and that list should be inserted into the final resulting list. Here is its signature:
  * *
  * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
  *
  */
object Exercise20 {

  def flatMap_[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B]) {
      (xs, acc) => append(f(xs), acc)
    }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def main(args: Array[String]) {
    println("flatMap(List(1,2,3))(i => List(i,i))  = " + flatMap_(List(1, 2, 3))(i => List(i, i)))
  }

}
