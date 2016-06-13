package chapter08

import chapter06.{RNG, State}
import chapter08.exercises._

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def unsized = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Exercise05.unit(a)

  def boolean: Gen[Boolean] = Exercise05.boolean

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Exercise04.choose(start, stopExclusive)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Exercise05.listOfN(n, g)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Exercise07.union(g1, g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)) = Exercise08.weighted(g1, g2)

  def listOf[A](g: Gen[A]): SGen[List[A]] = Exercise12.listOf(g)

  def listOf1[A](g: Gen[A]) = Exercise13.listOf1(g)

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

}
