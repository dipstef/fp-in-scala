package chapter03

import chapter03.exercises._

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = Exercise25.size(t)

  def maximum(t: Tree[Int]): Int = Exercise26.maximum(t)

  def depth[A](t: Tree[A]): Int = Exercise27.depth(t)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = Exercise28.map(t)(f)

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = Exercise29.fold(t)(f)(g)
}