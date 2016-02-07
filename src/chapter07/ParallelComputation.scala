package chapter07


trait ParallelComputation {
  type Par[A]

  def unit[A](a: => A): Par[A]

  def get[A](a: Par[A]): A

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]

  def fork[A](a: => Par[A]): Par[A]

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

}
