package c03_datastruct.exercises

import c03_datastruct.{Cons, List, Nil}


/**
  * As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
  *
  * For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
  *
  * You may have some difficulty finding a concise purely functional implementation that is also efficient. That’s okay.
  * Implement the function however comes most naturally.
  *
  * Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
  */
object Exercise24 {

  /* It's good to specify some properties about these functions up front.


  (xs append ys) startsWith xs
  xs startsWith Nil
  (xs append ys append zs) hasSubsequence ys
  xs hasSubsequence Nil */

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }

}
