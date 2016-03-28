package chapter07.exercises

import java.util.concurrent._

/**
  * Hard: Our non-blocking representation doesn’t currently handle errors at all. If at any point our computation
  * throws an exception, the run implementation’s latch never counts down and the exception is simply swallowed.
  * Can you fix that?”
  *
  */
object Exercise10 {

  // Try adding a second continuation argument to `Future.apply`, which takes an error handler.

  trait Future[+A] {
    final private[chapter07] def apply(k: A => Unit, onError: Throwable => Unit = throw _): Unit = {
      try {
        applyFun(k)
      } catch {
        case e: Throwable => onError(e)
      }

    }

    private[chapter07] def applyFun(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]


  def run[A](es: ExecutorService)(p: Par[A], onError: Throwable => Unit = throw _): A = {
    // A mutable, thread-safe reference, to use for storing the result
    val ref = new java.util.concurrent.atomic.AtomicReference[Either[A, Throwable]]
    // A latch which, when decremented, implies that `ref` has the result
    val latch = new CountDownLatch(1)
    // Asynchronously set the result, and decrement the latch
    p(es)(
      { a => ref.set(Left(a)); latch.countDown() }, { e => ref.set(Right(e)); latch.countDown() }
    )

    // Block until the `latch.countDown` is invoked asynchronously
    latch.await()
    // Once we've passed the latch, we know `ref` has been set, and return its value
    val result = ref.get
    result match {
      case Left(a) => a
      case Right(e) => onError(e); null.asInstanceOf[A]
    }
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def applyFun(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def applyFun(callBack: A => Unit): Unit =
      // forks evaluation and returns immediately. The callback will be invoked asynchronously by another thread.
        eval(es)(a(es)(callBack))
    }

  // helper function to evaluate an action asynchronously
  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(2)

    val b = run(es) {
      fork(unit(1))
    }

    println(b)
  }

}
