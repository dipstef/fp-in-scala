package c09_parsing.exercises

import c09_parsing.Parsers

/**
  * Can you think of any other primitives that might be useful for letting the programmer specify what error(s) in an or chain get reported?
  */
object Exercise11 {

  trait This[Parser[+ _]] extends Parsers[Parser] {

    /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
    def furthest[A](p: Parser[A]): Parser[A]

    /** In the event of an error, returns the error that occurred most recently. */
    def latest[A](p: Parser[A]): Parser[A]
  }


}
