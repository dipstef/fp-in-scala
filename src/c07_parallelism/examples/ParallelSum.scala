package c07_parallelism.examples

abstract class ParallelSum extends ParallelTrait {

  // Having map2 don't have it begin execution immediately implies a Par value is merely construction a description
  // of what needs to to be computed in parallel. Nothing happens until get is called.
  // The problem is that by constructing descriptions strictly, they'll be rather heavyweight objects, and occupy
  // much more space than the original list it self.
  // We should make map2 lazy and have it begin immediate execution of both sides in parallel.
  // This also addresses the problem of giving neither side priority over the other.


  // map2(unit(1), unit(1))(_ + _):
  // we know that the two computations we’re combining will execute so quickly that there isn’t much point in spawning
  // off a separate logical thread to evaluate them:
  // our API doesn’t give us any way of providing this sort of information: is very inexplicit about when computations
  // get forked off the main thread—the programmer doesn’t get to specify where this forking should occur.

  // We can make that explicit by defining: fork[A](a: => Par[A]): Par[A], which we can take to mean that the given Par
  // should be run in a separate logical thread:

  // fork solves the problem of instantiating our parallel computations too strictly, but more fundamentally it puts the
  // parallelism explicitly under programmer control.

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)

      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }
}
