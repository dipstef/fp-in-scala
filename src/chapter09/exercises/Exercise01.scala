package chapter09.exercises

import chapter09.Parsers

/**
  * Using product, implement the now-familiar combinator map2 and then use this to implement many1 in terms of many.
  *
  * Note that we could have chosen to make map2 primitive and defined product in terms of map2 as we’ve done in
  * previous chapters.
  *
  * The choice is up to you.
  */
object Exercise01 {

  trait This[Parser[+ _]] extends Parsers[Parser] {

    def map2_[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = map(product(p, p2))(f.tupled)

    def many1_[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)


  }

}
