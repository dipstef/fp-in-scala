package c10_monoids.exercises

import c10_monoids.Monoid
import c10_monoids.Monoid.foldMapV


/**
  * Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll need to come up with a creative Monoid.
  */
object Exercise09 {

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Our monoid tracks the minimum and maximum element seen so far as well as whether the elements are so far ordered.
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {

      override def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (a1, a2) match {
          case (Some((i1, j1, b1)), Some((i2, j2, b2))) => Some(i1 min i2, j1 max j2, b1 && b2 && j1 <= j2)
          case (x, None) => x
          case (None, x) => x
        }

      override def zero: Option[(Int, Int, Boolean)] = None
    }

    foldMapV(ints, mon)(i => Some(i, i, true)).forall(_._3)
  }

}
