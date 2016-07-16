package chapter04.exercises

import chapter04.Option

/**
  * Write a generic function map2 that combines two Option values using a binary function.
  *
  * If either Option value is None, then the return value is too.
  */
object Exercise03 {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

}
