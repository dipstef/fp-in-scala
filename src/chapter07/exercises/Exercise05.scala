package chapter07.exercises

import chapter07.Par.{Par, unit, fork, map, map2}

object Exercise05 {

  /**
    * Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
    */

  def sequence[A](as: List[Par[A]]): Par[List[A]] = {
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)
  }

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  private def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty)
      unit(Vector())
    else if (as.length == 1)
      map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      // concatenate all results
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }
}
