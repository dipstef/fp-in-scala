package chapter12.exercises

import chapter12.Monad

/**
  * Write a monad instance for Either.
  */
object Exercise05 {

  trait EitherMonad[E] extends Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  }

  def eitherMonad[E] = new EitherMonad[E]{}

}
