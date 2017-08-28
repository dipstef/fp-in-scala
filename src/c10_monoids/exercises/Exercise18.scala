package c10_monoids.exercises

import c10_monoids.Monoid.foldMapV
import c10_monoids.Monoids.mapMergeMonoid

/**
  * A bag is like a set, except that it’s represented by a map that contains one entry per element with that element
  * as the key, and the value under that key is the number of times the element appears in the bag.
  *
  * For example:
  *
  * scala> bag(Vector("a", "rose", "is", "a", "rose"))
  *
  *  res0: Map[String,Int] = Map(a -> 2, rose -> 2, is -> 1)”
  */
object Exercise18 {

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](Exercise01.intAddition))((a: A) => Map(a -> 1))


}
