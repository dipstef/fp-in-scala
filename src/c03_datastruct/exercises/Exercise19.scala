package c03_datastruct.exercises

import c03_datastruct.{Cons, List, Nil}
import c03_datastruct.List.foldRight

import scala.collection.mutable.ListBuffer

/**
  * Write a function filter that removes elements from a list unless they satisfy a given predicate.
  * Use it to remove all odd numbers from a List[Int].
  */
object Exercise19 {

  def filter_[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

}
