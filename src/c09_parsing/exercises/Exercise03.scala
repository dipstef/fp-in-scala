package c09_parsing.exercises

import c09_parsing.Parsers

/**
  * Hard: Before continuing, see if you can define many in terms of or, map2, and succeed.
  */
object Exercise03 {

  trait This[Parser[+ _]] extends Parsers[Parser] {

    def many_[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  }
}
