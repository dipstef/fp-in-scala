package chapter04


import chapter04.exercises.Exercise03

import scala.{Either => _, Option => _, Some => _, _}

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter


sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  /* Or via explicit pattern matching. */
  def filter_1(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }


}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = Exercise03.map2(a, b)(f)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = sys.error("todo")

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}