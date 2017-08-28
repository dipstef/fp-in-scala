package chapter12

import chapter06.State
import chapter11.Monad
import chapter11.exercises.Exercise02.StateMonads

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {

  trait StateMonad[S] extends  Monad[({type lambda[x] = State[S, x]})#lambda] {

    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f

  }

  trait EitherMonad[E] extends Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  }

  // Exercise 02
  def stateMonad[S] = new StateMonads[S]{}
  // Exercise 05
  def eitherMonad[E] = new EitherMonad[E]{}

  // Exercise 20
  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[({type f[x] = G[H[x]]})#f] =
    new Monad[({type f[x] = G[H[x]]})#f] {
      def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

      override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
    }

}