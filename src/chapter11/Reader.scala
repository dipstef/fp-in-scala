package chapter11

// Similar to how `State` passes along a state, except that in `Reader` the "state" is read-only.
case class Reader[R, A](run: R => A)

trait ReaderMonad[R] extends Monad[({type f[x] = Reader[R, x]})#f] {

  def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

  // The action of Reader's `flatMap` is to pass the `r` argument along to both the outer Reader and also to the result
  // of `f`, the inner Reader.
  def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader( r => f(st.run(r)).run(r))

  // A primitive operation for it would be simply to ask for the `R` argument:
  def ask: Reader[R, R] = Reader(r => r)

  // The meaning of `sequence` here is that if you have a list of functions, you can turn it into a function that takes
  // one argument and passes it to all the functions in the list, returning a list of the results.
  override def sequence[A](lma: List[Reader[R, A]]): Reader[R, List[A]] = super.sequence(lma)

  // The meaning of `join` is simply to pass the same value as both arguments to a binary function.
  override def join[A](mma: Reader[R, Reader[R, A]]): Reader[R, A] = super.join(mma)

  // The meaning of `replicateM` is to apply the same function a number of times to the same argument, returning a
  // list of the results. Note that if this function is _pure_, (which it should be), this can be exploited by only
  // applying the function once and replicating the result instead of calling the function many times.

  // This means the Reader monad can override replicateM to provide a very efficient implementation.
  override def replicateM[A](n: Int, ma: Reader[R, A]): Reader[R, List[A]] = super.replicateM(n, ma)
}