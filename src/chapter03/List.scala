package chapter03

import chapter03.exercises._

sealed trait List[+A]

// `List` data type, parametrised on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = Exercise02.tail(l)

  def setHead[A](l: List[A], h: A): List[A] = Exercise03.setHead(l, h)

  def drop[A](l: List[A], n: Int): List[A] = Exercise04.drop(l, n)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = Exercise05.dropWhile(l, f)

  def init[A](l: List[A]): List[A] = Exercise06.init(l)

  def length[A](l: List[A]): Int = Exercise09.length(l)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = Exercise10.foldLeft(l, z)(f)

  def map[A, B](l: List[A])(f: A => B): List[B] = Exercise18.map(l)(f)

  def reverse[A](l: List[A]): List[A] = Exercise12.reverse(l)

  def filter[A](l: List[A])(f: A => Boolean): List[A] = Exercise19.filter(l)(f)

  def concat[A](l: List[List[A]]): List[A] = Exercise15.concat(l)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = Exercise20.flatMap(l)(f)

}
