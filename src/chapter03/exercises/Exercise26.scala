package chapter03.exercises

import chapter03.{Branch, Leaf, Tree}

/**
  * Write a function maximum that returns the maximum element in a Tree[Int].
  *
  * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
  */
object Exercise26 {

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

}
