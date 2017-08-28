package c07_parallelism.examples

trait ParallelTrait {
  // Par itself doesn’t need to know how to actually implement the parallelism. It’s more a description of a parallel
  // computation that gets interpreted at a later time

  type Par[A]

  // promotes a constant value to a parallel computation.
  def unit[A](a: => A): Par[A]

  // combines the results of two parallel computations with a binary function
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  // marks a computation for concurrent evaluation. The evaluation won’t actually occur until forced by run
  def fork[A](a: => Par[A]): Par[A]

  // derived combinator: wraps its unevaluated argument in a Par and marks it for concurrent evaluation
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // extracts a value from a Par by actually performing the computation
  def run[A](a: Par[A]): A

}
