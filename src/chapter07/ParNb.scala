package chapter07

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import chapter07.exercises.{Exercise11, Exercise12, Exercise13, Exercise14}


object ParNb {

  trait Future[+A] {
    // Use of a common technique of using side effects as an implementation detail for a purely functional API.
    // We can get away with this because the side effects we use are not observable to code that uses Par.”

    // callback or continuation
    private[chapter07] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]


  def run[A](es: ExecutorService)(p: Par[A]): A = {
    // A mutable, thread-safe reference, to use for storing the result
    val ref = new java.util.concurrent.atomic.AtomicReference[A]
    // A latch which, when decremented, implies that `ref` has the result
    val latch = new CountDownLatch(1)
    // Asynchronously set the result, and decrement the latch
    p(es) { a => ref.set(a); latch.countDown() }
    // Block until the `latch.countDown` is invoked asynchronously
    latch.await()
    // Once we've passed the latch, we know `ref` has been set, and return its value
    ref.get
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
        // Two mutable vars are used to store two results
        var ar: Option[A] = None
        var br: Option[B] = None

        // An Actor awaits both results, combines them with f and passes the results to cb
        val combiner = Actor[Either[A, B]](es) {
          // If the A result came in first, stores it in ar and waits for the B.
          // If the A result came last and we already have B, calls f with both results and passes the resulting C to the callback cb.
          case Left(a) =>
            br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
          // Analogously
          case Right(b) =>
            ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
        }
        // Passes the actor as continuation to both sides
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  // specialized version of `map`
  def map[A, B](p: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit =
        p(es)(a => eval(es) {
          cb(f(a))
        })
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
    }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = sequence(as.map(asyncF(f)))

  def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] = sequenceBalanced(as.map(asyncF(f)))

  // A computation that proceeds with t if cond results in true, or f if cond results in false
  def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        p(es) { b =>
          if (b) eval(es) {
            t(es)(cb)
          }
          else eval(es) {
            f(es)(cb)
          }
        }
    }

  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] = Exercise11.choiceN(p)(ps)

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]) =
    Exercise11.choiceViaChoiceN(a)(ifTrue, ifFalse)

  def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] = Exercise12.choiceMap(p)(ps)

  def chooser[A, B](p: Par[A])(choices: A => Par[B]): Par[B] = flatMap(p)(choices)

  /*
    flatMap is suggestive of the fact that this operation could be decomposed into two steps: mapping f: A => Par[B]
    over our Par[A], which generates a Par[Par[B]] and then flattening this nested Par[Par[B]] to a Par[B].

    But this is interesting, it suggests that all we needed to do was add an even simpler combinator, let’s call it join,
    for converting a Par[Par[X]] to Par[X] for any choice of X
   */
  def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = Exercise13.flatMap(p)(f)

  def join[A](p: Par[Par[A]]): Par[A] = Exercise14.join(p)

  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

}