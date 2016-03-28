package chapter07

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object ParNb {

  trait Future[+A] {
    // Use of a common technique of using side effects as an implementation detail for a purely functional API.
    // We can get away with this because the side effects we use are not observable to code that uses Par.”
    private[chapter07] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]


  def run[A](es: ExecutorService)(p: Par[A]): A = {
    // A mutable, thread-safe reference, to use for storing the result
    val ref = new java.util.concurrent.atomic.AtomicReference[A]
    // A latch which, when decremented, implies that `ref` has the result
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown() } // Asynchronously set the result, and decrement the latch
    latch.await() // Block until the `latch.countDown` is invoked asynchronously
    ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(callBack: A => Unit): Unit =
      // forks evaluation and returns immediately. The callback will be invoked asynchronously by another thread.
        eval(es)(a(es)(callBack))
    }

  // helper function to evaluate an action asynchronously
  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })


  // We’d like map2 to run both Par arguments in parallel. When both results have arrived, we want to invoke f and then
  // pass the resulting C to the continuation
  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        // An Actor is essentially a concurrent process that doesn’t constantly occupy a thread. Instead, it only occupies a
        // thread when it receives a message.
        // Multiple threads may be concurrently sending messages to an actor, the actor processes only one message at a
        // time, queueing other messages for subsequent processing
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(es)(cb(f(a, br.get)))
            else ar = Some(a)
          case Right(b) =>
            if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
            else br = Some(b)
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

}
