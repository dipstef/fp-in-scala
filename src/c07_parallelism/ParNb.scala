package c07_parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}


object ParNb {

  trait Future[+A] {
    // Use of a common technique of using side effects as an implementation detail for a purely functional API.
    // We can get away with this because the side effects we use are not observable to code that uses Par.”

    // callback or continuation
    private[c07_parallelism] def apply(k: A => Unit): Unit
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
      def call: Unit = r
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

  // Exercise 11
  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit =
      p(es)(ind =>
        eval(es) {
          ps(ind)(es)(cb)
        }
      )
  }

  // Exercise 11
  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

  // Exercise 12
  def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =  es => new Future[V] {
    def apply(cb: (V) => Unit): Unit = p(es)(k => ps(k)(es)(cb))
  }

  def chooser[A, B](p: Par[A])(choices: A => Par[B]): Par[B] = flatMap(p)(choices)

  /*
    flatMap is suggestive of the fact that this operation could be decomposed into two steps: mapping f: A => Par[B]
    over our Par[A], which generates a Par[Par[B]] and then flattening this nested Par[Par[B]] to a Par[B].

    But this is interesting, it suggests that all we needed to do was add an even simpler combinator, let’s call it join,
    for converting a Par[Par[X]] to Par[X] for any choice of X
   */
  // Exercise 13
  def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = {
    es => new Future[B]{
      def apply(cb: B => Unit): Unit =
        p(es)(a => f(a)(es)(cb))
    }
  }
  // Exercise 14
  def join[A](p: Par[Par[A]]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit =
      p(es)(
        p2 => eval(es) {
          p2(es)(cb)
        }
      )
  }

  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  // infix versions of `map`, `map2` and `flatMap`
  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = ParNb.map(p)(f)
    def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = ParNb.map2(p,b)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = ParNb.flatMap(p)(f)
    def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
  }

}