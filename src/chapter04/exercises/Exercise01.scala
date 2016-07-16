package chapter04.exercises

import scala.{Option => _, Some => _, Either => _, _}

/**
  * Implement all of the preceding functions on Option. As you implement each function, try to think about what it means
  * and in what situations you’d use it. We’ll explore when to use each of these functions next.
  *
  * Here are a few hints for solving this exercise:
  *
  * It’s fine to use pattern matching, though you should be able to implement all the functions besides map and getOrElse
  * without resorting to pattern matching.
  *
  * For map and flatMap, the type signature should be enough to determine the implementation.
  *
  * getOrElse returns the result inside the Some case of the Option, or if the Option is None, returns the given default value.
  * orElse returns the first Option if it’s defined; otherwise, it returns the second Option.
  *
  */
object Exercise01 {

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

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

  def main(args: Array[String]) {
    val one = Some(1)

    println("Some(1).map(_ + 1) = " + one.map(_ + 1))
    println("Some(1).flatMap(_ => Some(2))) = " + one.flatMap(_ => Some(2)))
    println("Some(1).filter(_ % 2 == 0) = " + one.filter(_ % 2 == 0))
    println("Some(1).getOrElse(2) = " + one.getOrElse(2))
    println("Some(1).orElse(Option(2)) = " + one.orElse(Some(2)))

  }

}
