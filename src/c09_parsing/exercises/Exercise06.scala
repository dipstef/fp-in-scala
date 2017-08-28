package c09_parsing.exercises

import c09_parsing.Parsers

/**
  * Suppose we want to parse a single digit, like '4', followed by that many 'a' characters (this sort of problem
  * should feel familiar from previous chapters).
  *
  * Examples of valid input are "0", "1a", "2aa", "4aaaa", and so on. This is an example of a context-sensitive grammar.
  * It can’t be expressed with product because our choice of the second parser depends on the result of the first
  * (the second parser depends on its context).
  *
  * We want to run the first parser, and then do a listOfN using the number extracted from the first parser’s result
  *
  * “Using flatMap and any other combinators, write the context-sensitive parser we couldn’t express earlier.
  *
  * To parse the digits, you can make use of a new primitive, regex, which promotes a regular expression to a Parser.[10]
  *
  * In Scala, a string s can be promoted to a Regex object (which has methods for matching) using s.r, for instance, "[a-zA-Z_][a-zA-Z0-9_]*".r.
  *
  * 10 In theory this isn’t necessary; we could write out "0" | "1" | ... "9" to recognize a single digit,
  * but this isn’t likely to be very efficient.
  */
object Exercise06 {

  trait This[Parser[+ _]] extends Parsers[Parser] {

    def parse(): Parser[Int] =
      for {
        digit <- "[0-9]+".r
        n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
        _ <- listOfN(n, char('a'))
      } yield n


  }

}
