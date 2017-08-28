package c09_parsing.exercises

import c09_parsing.Parsers

/**
  * Implement product and map2 in terms of flatMap.
  */
object Exercise07 {

  trait This[Parser[+ _]] extends Parsers[Parser] {

    def map2_[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {a <- p; b <- p2} yield f(a, b)

    def product_[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = for {a <- p; b <- p2} yield (a, b)


  }

}
