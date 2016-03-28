package chapter07.examples

abstract class ParallelSumSequential extends ParallelTrait {
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = unit(sum(l))
      val sumR: Par[Int] = unit(sum(r))
      // This actually should avoided as run forces the side effects to happen, and as such we can't combine
      // parallel computations, as in this case get(sumL) will block before we can call get(sumR)
      run(sumL) + run(sumR)
    }
}
