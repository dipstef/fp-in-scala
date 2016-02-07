package chapter07


trait ParallelComputation {
  type Par[A]

  def unit[A](a: => A): Par[A]

  def get[A](a: Par[A]): A

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]

}

abstract class ParallelSum extends ParallelComputation {

  def sumSequential(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = unit(sumSequential(l))
      val sumR: Par[Int] = unit(sumSequential(r))
      // This actually should avoided as get forces the side effects to happen, and as such we can't combine
      // parallel computations, as in this case get(sumL) will block before we can call get(sumR)
      get(sumL) + get(sumR)
    }

  // Because map2 is strict, and Scala evaluates arguments left to right, whenever we encounter map2(sum(x),sum(y))(_ + _),
  // we have to then evaluate sum(x) and so on recursively.
  // This has as consequence that we’ll strictly construct the entire left half of the tree of summations first before
  // moving on to (strictly) constructing the right half

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)

      map2(sum(l), sum(r))(_+_)
    }

}

object Examples {

  // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists,
  // these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  def main(args: Array[String]) {
    val v = Vector(1, 2, 3, 4, 5)

    println("v = " + v)
    println("sum(v) = " + sum(v))
  }

}
