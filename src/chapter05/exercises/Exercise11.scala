package chapter05.exercises

import chapter05.Stream
import chapter05.Stream.{cons, empty}

/**
  * Write a more general stream-building function called unfold.
  *
  * It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
  */
object Exercise11 {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

}
