package chapter11

import language.higherKinds


trait Monad[M[_]] extends Functor[M] {
  self =>

  implicit def operators[A](m: M[A]): MonadOps[A] = MonadOps[A](m)

  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    // flatMap(ma)(a => map(mb)(b => f(a, b)))
    for {a <- ma; b <- mb} yield f(a,b)

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]())) { (ma, acc) => map2(ma, acc)(_ :: _) }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]())) { (a, acc) => map2(f(a), acc)(_ :: _) }

  def product[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  // It repeats the `ma` monadic value `n` times and gathers the results in a single value, where the monad `M`
  // determines how values are actually combined.
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
    case Nil => unit(List[A]())
    case x :: xs => map2(f(x), filterM(xs)(f)) { (p, fxs) => if (p) x :: fxs else fxs }
  }

  def compose[A, B, C](f: (A) => M[B], g: (B) => M[C]): (A) => M[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = compose((_: Unit) => ma, f)()

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
  // Implement in terms of `join`:
  def __compose[A, B, C](f: (A) => M[B], g: (B) => M[C]): (A) => M[C] = a => join(map(f(a))(g))


  case class MonadOps[A](m: M[A]) {

    def flatMap[B](f: A => M[B]): M[B] = self.flatMap(m)(f)
    def map[B](f: A => B): M[B] = self.map(m)(f)

  }

}

