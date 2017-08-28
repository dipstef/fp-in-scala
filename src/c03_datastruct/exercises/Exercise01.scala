package c03_datastruct.exercises

import c03_datastruct.List._
import c03_datastruct.{Cons, List, Nil}

/**
  * What will be the result of the following match expression?
  */
object Exercise01 {
  val l = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def main(args: Array[String]) {
    assert(l == 3)
  }

}
