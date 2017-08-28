package c07_parallelism.examples

abstract class ParallelSumStrict extends ParallelTrait {
  // Because map2 is strict, and Scala evaluates arguments left to right, whenever we encounter map2(sum(x),sum(y))(_ + _),
  // we have to then evaluate sum(x) and so on recursively.
  // This has as consequence that we’ll strictly construct the entire left half of the tree of summations first before
  // moving on to (strictly) constructing the right half

  def _sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)

      map2(_sum(l), _sum(r))(_ + _)
    }
}
