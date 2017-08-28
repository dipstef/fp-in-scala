package c03_datastruct.exercises

import c03_datastruct.{Branch, Leaf, Tree}

/**
  * Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with
  * a given function.
  */
object Exercise28 {

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

}
