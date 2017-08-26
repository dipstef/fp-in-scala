package chapter09.exercises

import chapter09.Parsers

/**
  * map is no longer primitive. Express it in terms of flatMap and/or other combinators.
  */
object Exercise08 {

  trait This[Parser[+ _]] extends Parsers[Parser] {

    def map_[A, B](a: Parser[A])(f: A => B): Parser[B] =
      // p.flatMap(a => succeed(f(a)))
      flatMap(a)(f andThen succeed)


  }

}
