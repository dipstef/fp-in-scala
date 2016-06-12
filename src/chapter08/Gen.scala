package chapter08

import chapter06.{RNG, State}
import chapter08.exercises.{Exercise04, Exercise05, Exercise07, Exercise08}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Exercise05.unit(a)

  def boolean: Gen[Boolean] = Exercise05.boolean

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Exercise04.choose(start, stopExclusive)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Exercise05.listOfN(n, g)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Exercise07.union(g1, g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)) = Exercise08.weighted(g1, g2)
}
