package chapter10.exercises

import chapter10.Monoid.foldMap
import chapter10.Monoids.{dual, endoMonoid}

/**
  * Hard: The foldMap function can be implemented using either foldLeft or foldRight. But you can also write foldLeft
  * and foldRight using foldMap! Try it.
  */
object Exercise06 {

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B]){f.curried}(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, dual(endoMonoid[B])){f.curried}(z)


}
