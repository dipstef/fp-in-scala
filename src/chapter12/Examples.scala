package chapter12

object Examples {

  // Not a monad: (possibly infinite) streams. We can define map2 and unit for these streams, but not flatMap:
  trait StreamApplicative extends Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                          f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  val streamApplicative = new StreamApplicative {}


}
