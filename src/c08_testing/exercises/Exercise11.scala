package c08_testing.exercises

import c08_testing.Gen

/**
  * Not surprisingly, SGen at a minimum supports many of the same operations as Gen, and the implementations are rather
  * mechanical.
  * *
  * Define some convenience functions on SGen that simply delegate to the corresponding functions on Gen
  */
object Exercise11 {

  case class SGen[+A](g: Int => Gen[A]) {
    def apply(n: Int): Gen[A] = g(n)

    def map[B](f: A => B): SGen[B] =
      SGen {
        g(_) map f
      }

    def flatMap[B](f: A => SGen[B]): SGen[B] = {
      val g2: Int => Gen[B] = n => {
        g(n) flatMap {
          f(_).g(n)
        }
      }
      SGen(g2)
    }

    def **[B](s2: SGen[B]): SGen[(A, B)] =
      SGen(n => apply(n) ** s2(n))
  }


}
