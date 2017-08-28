package c05_laziness.exercises

import c05_laziness.Stream
import c05_laziness.Stream.cons

/**
  * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  */
object Exercise10 {

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

}
