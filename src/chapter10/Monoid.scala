package chapter10

import chapter07.ParNb
import chapter07.ParNb.Par
import chapter10.exercises._

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {


  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero: A = m.zero
  }

  def endoMonoid[A]: Monoid[A => A] = Exercise03.endoMonoid

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  // But what if our list has an element type that doesn’t have a Monoid instance? Well, we can always map over the list
  // to turn it into a type that does:”
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero: Par[A] = ParNb.unit(m.zero)

    def op(a: Par[A], b: Par[A]): Par[A] = a.map2(b)(m.op)
  }

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    ParNb.parMap(v)(f).flatMap { bs => foldMapV(bs, par(m))(b => ParNb.lazyUnit(b))
    }


}



