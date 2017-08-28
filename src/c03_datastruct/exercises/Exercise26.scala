package c03_datastruct.exercises

import c03_datastruct.{Branch, Leaf, Tree}

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
