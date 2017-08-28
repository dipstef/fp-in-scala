package c07_parallelism.exercises

import c07_parallelism.Par.{Par, lazyUnit}

/**
  * using lazyUnit, write a function to convert any function A => B to one that evaluates its result asynchronously.
  */

object Exercise04 {

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))


}
