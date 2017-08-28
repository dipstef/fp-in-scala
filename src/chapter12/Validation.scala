package chapter12


sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector.empty)
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


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

object Validation {

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new ValidationApplicative[E] {}

}