package chapter08.exaustive

import chapter08.exaustive.Gen.{Sized, Unsized}

trait SGen[+A] {
  def map[B](f: A => B): SGen[B] = this match {
    case Sized(g) => Sized(g andThen (_ map f))
    case Unsized(g) => Unsized(g map f)
  }
  def flatMap[B](f: A => Gen[B]): SGen[B] = this match {
    case Sized(g) => Sized(g andThen (_ flatMap f))
    case Unsized(g) => Unsized(g flatMap f)
  }
  def **[B](s2: SGen[B]): SGen[(A,B)] = (this,s2) match {
    case (Sized(g), Sized(g2)) => Sized(n => g(n) ** g2(n))
    case (Unsized(g), Unsized(g2)) => Unsized(g ** g2)
    case (Sized(g), Unsized(g2)) => Sized(n => g(n) ** g2)
    case (Unsized(g), Sized(g2)) => Sized(n => g ** g2(n))
  }
}
