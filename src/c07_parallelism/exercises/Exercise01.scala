package c07_parallelism.exercises

import c07_parallelism.Par.Par

/**
  * Par.map2 is a new higher-order function for combining the result of two parallel computations. What is its signature?
  * Give the most general signature possible (don’t assume it works only for Int).
  */

trait Exercise01 {
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
}
