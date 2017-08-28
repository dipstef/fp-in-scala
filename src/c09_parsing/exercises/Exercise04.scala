package c09_parsing.exercises

import c09_parsing.Parsers

/**
  * Hard: Using map2 and succeed, implement the listOfN combinator from earlier.
  */
object Exercise04 {

  trait This[Parser[+ _]] extends Parsers[Parser] {

    def listOfN_[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= 0) succeed(List())
      else map2(p, listOfN(n-1, p))(_ :: _)


  }

}
