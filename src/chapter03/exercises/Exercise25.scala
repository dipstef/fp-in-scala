package chapter03.exercises

import chapter03.{Branch, Leaf, Tree}

/**
  * Write a function size that counts the number of nodes (leaves and branches) in a tree.
  */
object Exercise25 {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
}
