package c11_monads.exercises

import c07_parallelism.ParNb
import c07_parallelism.ParNb.Par
import c09_parsing.Parsers
import c11_monads.Monad

/**
  * Write monad instances for Par, Parser, Option, Stream, and List.
  */
object Exercise01 {

  // You have already defined `unit` and `flatMap` for these types. The solution is to simply call them from your `Monad` implementation.

  val listMonad: Monad[List] = new Monad[List] {

    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)
  }

  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = ParNb.unit(a)
    override def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] = ParNb.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)

    override def flatMap[A,B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    override def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    override def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }


}
