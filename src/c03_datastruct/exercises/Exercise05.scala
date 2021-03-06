package c03_datastruct.exercises

import c03_datastruct.{Cons, List}

/**
  * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
  */
object Exercise05 {

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    case _ => l
  }

}
