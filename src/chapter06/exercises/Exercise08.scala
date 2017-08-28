package chapter06.exercises

import chapter06.RNG._


/**
  * Implement flatMap, and then use it to implement nonNegativeLessThan.
  */
object Exercise08 {

  object Notes {
    // First stab on implementing:
    // will certainly generate a number in the range, but it’ll be skewed because Int.MaxValue may not be exactly
    // divisible by n. So numbers that are less than the remainder of that division will come up more frequently

    def _nonNegativeLessThan(n: Int): Rand[Int] = map(nonNegativeInt) {
      _ % n
    }

    /* Moving in the right direction but wrong type
    def _nonNegativeLessThan_1(n: Int): Rand[Int] = {
      map(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) mod else _nonNegativeLessThan_1(n)
      }
    } */

    // Wwe would like is to chain things together so that the RNG that’s returned by nonNegativeInt is passed along to
    // the recursive call to nonNegativeLessThan.
    // It would be better to have a combinator that does this passing along for us

    def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }


  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def main(args: Array[String]) {
    println("flatMap example: " + flatMap(nonNegativeInt){ i => unit(i)}(Simple(123)))

    for( i <- 1 to 10) {
      println("nonNegativeLessThan(10)(rng) = " + nonNegativeLessThan(10)(Simple(i)))
    }
  }
}
