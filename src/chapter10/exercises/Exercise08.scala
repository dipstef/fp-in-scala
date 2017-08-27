package chapter10.exercises

import chapter07.ParNb.{unit, parMap, lazyUnit}
import chapter07.ParNb.Par
import chapter10.Monoid
import chapter10.Monoid.foldMapV

/**
  * Hard: Also implement a parallel version of foldMap using the library we developed in chapter 7. Hint:
  *
  * Implement par, a combinator to promote Monoid[A] to a Monoid [Par[A]],[5] and then use this to implement parFoldMap.
  *
  * 5 The ability to “lift” a Monoid into the Par context is something we’ll discuss more generally in chapters 11 and 12.
  */
object Exercise08 {

  // This ability to 'lift' a monoid any monoid to operate within some context (here `Par`) is something we'll discuss more in
  // chapters 11 & 12
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero: Par[A] = unit(m.zero)

    def op(a: Par[A], b: Par[A]): Par[A] = a.map2(b)(m.op)
  }

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    parMap(v)(f).flatMap { bs => foldMapV(bs, par(m))(b => lazyUnit(b))}


}
