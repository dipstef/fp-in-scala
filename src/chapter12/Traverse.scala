package chapter12

import chapter06.State
import chapter06.State.{get => getState, set => setState}
import chapter10.{Foldable, Monoid}
import chapter11.Functor
import Applicative.{Const, monoidApplicative}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
  // Compiles but reported as error IntelliJ
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] = traverse(fma)(identity)

  // Exercise 14
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    type Id[A_] = A_

    // using the Monad instance as implicit val
    traverse[Id, A, B](fa)(f)(
      new Monad[Id] {
        override def unit[A_](a: => A_): A_ = a

        override def flatMap[A_, B_](a: A_)(f: A_ => B_): B_ = f(a)
      }
    )
  }

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- getState[S]
      (b, s2) = f(a, s1)
      _ <- setState(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // Exercise 16
  def reverse[A](fa: F[A]): F[A]  =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  // Exercise 17
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  // Exercise 18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  // Exercise 19
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }

}
