package c06_state.exercises

import c06_state.RNG.{Rand, flatMap, unit}

/**
  * Reimplement map and map2 in terms of flatMap. The fact that this is possible is what we’re referring to when we say
    that flatMap is more powerful than map and map2
  */
object Exercise09 {
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}
