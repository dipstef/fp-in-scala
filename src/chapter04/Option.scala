package chapter04


sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  /* Or via explicit pattern matching. */
  def filter_1(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  // Exercise 03
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  // Exercise 05
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  // Exercise 05
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

}

object Options {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 02
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

}
