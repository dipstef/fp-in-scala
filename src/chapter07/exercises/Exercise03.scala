package chapter07.exercises

import java.util.concurrent.{TimeUnit, Future, ExecutorService}

import chapter07.Par.Par

/**
  * “Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.”
  */
object Exercise03 {

  /**
    * Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
    * We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of
    * pure values won't affect results).
    */
  case class Map2Future[A, B, C](a: Future[A],
                                 b: Future[B],
                                 f: (A, B) => C) extends Future[C] {

    @volatile var cache: Option[C] = None

    override def isDone = cache.isDefined

    override def isCancelled = a.isCancelled || b.isCancelled

    override def cancel(evenIfRunning: Boolean) = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    override def get = compute(Long.MaxValue)

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

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val (af, bf) = (a(es), b(es))
    Map2Future(af, bf, f)
  }

}
