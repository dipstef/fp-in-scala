package c03_datastruct.exercises

import c03_datastruct.{Cons, List, Nil}
import c03_datastruct.List.foldRight

import scala.collection.mutable.ListBuffer

/**
  * Write a function map that generalizes modifying each element in a list while maintaining the structure of the list”
  */
object Exercise18 {

  def map_[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  /*
    `map` will be implemented using local mutation. Again, note that the mutation isn't observable
     outside the function, since we're only mutating a buffer that we've allocated.
   */
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }
}
