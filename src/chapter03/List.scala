package chapter03

import chapter03.exercises.Exercise05.dropWhile
import chapter03.exercises.Exercise10.foldLeft
import chapter03.exercises._

import scala.collection.mutable.ListBuffer

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

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 02
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 03
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  // Exercise 04
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  // Exercise 05
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    case _ => l
  }

  // Exercise 06
  def init[A](l: List[A]): List[A] = {
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def collect(l: List[A]) : List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => List(buf.toList: _*) // Necessary because we use our own List implementation
      case Cons(x, xs) =>
        buf += x
        collect(xs)
    }
    collect(l)
  }

  // Exercise 09
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  // Exercise 10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 18
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  // Exercise 12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]()){case (acc, x) => Cons(x, acc)}

  // Exercise 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  // Exercise 15
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  // Exercise 20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

}
