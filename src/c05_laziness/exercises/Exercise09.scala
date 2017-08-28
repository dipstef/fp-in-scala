package c05_laziness.exercises

import c05_laziness.Stream
import c05_laziness.Stream.cons

/**
  * Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.[7]
  *
  */
object Exercise09 {

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

}
