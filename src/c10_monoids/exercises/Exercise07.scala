package c10_monoids.exercises

import c10_monoids.Monoid

/**
  * Implement a foldMap for IndexedSeq.[4] Your implementation should use the strategy of splitting the sequence in two,
  * recursively processing each half, and then adding the answers together with the monoid.
  *
  * 4 Recall that IndexedSeq is the interface for immutable data structures supporting efficient random access.
  * It also has efficient splitAt and length methods.
  *
  */
object Exercise07 {

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v.head)
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

}
