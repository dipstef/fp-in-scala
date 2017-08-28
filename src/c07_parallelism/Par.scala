package c07_parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future`
  // that just wraps a constant value. It doesn't use the `ExecutorService` at all.
  // It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having
  // `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))`
  // if we want the evaluation of `f` to occur in a separate thread.
  private def _map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both
    // `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a
    // `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of
    // time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    UnitFuture(f(af.get, bf.get))
  }

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one,
  // the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread
  // in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some
  // potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more
  // serious problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  // derived combinator: wraps its unevaluated argument in a Par and marks it for concurrent evaluation
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // trick to add infix syntax to any type using implicit conversions
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {}


  // this version of map2 allows timeouts
  // Exercise 0
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val (af, bf) = (a(es), b(es))
    Map2Future(af, bf, f)
  }

  // Exercise 04
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  // map can be implemented in terms of map2, but not the other way around, just shows that map2 is strictly more powerful
  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  // returns a sorted result
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  // collects n parallel results
  // Exercise 05
  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)


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

  // parallel map
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }


}

/**
  * Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
  * We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of
  * pure values won't affect results).
  */
case class Map2Future[A, B, C](a: Future[A],
                               b: Future[B],
                               f: (A, B) => C) extends Future[C] {

  @volatile var cache: Option[C] = None

  override def isDone: Boolean = cache.isDefined

  override def isCancelled: Boolean = a.isCancelled || b.isCancelled

  override def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

  override def get: C = compute(Long.MaxValue)

  override def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, units))

  private def compute(timeoutInNanos: Long): C = cache match {
    case Some(c) => c
    case None =>
      val start = System.nanoTime
      val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime
      val aTime = stop - start
      val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
      val ret = f(ar, br)
      cache = Some(ret)
      ret
  }
}
