package chapter06.exercises

import chapter06.RNG.{Rand, flatMap, unit}

/**
  * Created by dipstef on 31/01/2016.
  */
object Exercise09 {
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}
