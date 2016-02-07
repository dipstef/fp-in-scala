package chapter07


trait ParallelComputation {
  type Par[A]

  def unit[A](a: => A): Par[A]

  def get[A](a: Par[A]): A

}

abstract class ParallelSum extends ParallelComputation {

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = unit(sum(l))
      val sumR: Par[Int] = unit(sum(r))
      get(sumL) + get(sumR)
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
