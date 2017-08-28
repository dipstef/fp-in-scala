package chapter12.exercises

import chapter12.{Applicative, Failure, Success, Validation}

/**
  * Write an Applicative instance for Validation that accumulates errors in Failure. Note that in the case of Failure
  * there’s always at least one error, stored in head. The rest of the errors accumulate in the tail.
  */
object Exercise06 {


  trait ValidationApplicative[E] extends Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A) = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) =>
          Failure(h1, t1 ++ Vector(h2) ++ t2)
        case (e@Failure(_, _), _) => e
        case (_, e@Failure(_, _)) => e
      }
  }

}
