package c03_datastruct.exercises

import c03_datastruct.{Branch, Leaf, Tree}

/**
  * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  */
object Exercise27 {

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

}
