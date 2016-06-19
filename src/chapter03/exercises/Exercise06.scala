package chapter03.exercises

import chapter03.{Cons, List, Nil}

import scala.collection.mutable.ListBuffer


/**
  * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the
  * last element of a List.
  *
  * So, given List(1,2,3,4), init will return List(1,2,3).
  *
  * Why can’t this function be implemented in constant time like tail?
  */
object Exercise06 {

  /*
    we're copying the entire list up until the last element. Besides being inefficient, the natural recursive solution
    will use a stack frame for each element of the list, which can lead to stack overflows for large lists.
   */
  def init_[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init_(xs))
  }

  /*
    With lists, it's common to use a temporary, mutable buffer internal to the function (with lazy lists or streams,
    we don't normally do this).

    So long as the buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
   */

  def init[A](l: List[A]): List[A] = {
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def collect(l: List[A]) : List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => List(buf.toList: _*) // Necessary because we use our own List implementation
      case Cons(x, xs) =>
        buf += x
        collect(xs)
    }
    collect(l)
  }

}