package chapter07


trait ParallelComputation {
  // Par itself doesn’t need to know how to actually implement the parallelism. It’s more a description of a parallel
  // computation that gets interpreted at a later time

  type Par[A]

  def unit[A](a: => A): Par[A]

  def run[A](a: Par[A]): A

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]

  def fork[A](a: => Par[A]): Par[A]

  // lazyUnit is a combinator, as opposed to a primitive combinator like unit. We can define lazyUnit in terms of other
  // operations.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

}
