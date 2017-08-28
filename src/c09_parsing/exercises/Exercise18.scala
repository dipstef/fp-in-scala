package c09_parsing.exercises

import c09_parsing.instances.Location
import c09_parsing.instances.ReferenceTypes.ParseState
import c09_parsing.{Parsers, ParsingError}

/**
  * Some information is lost when we combine parsers with the or combinator. If both parsers fail, we’re only keeping
  * the errors from the second parser.
  *
  * But we might want to show both error messages, or choose the error from whichever branch got furthest without failing.
  *
  * Change the representation of ParseError to keep track of errors that occurred in other branches of the parser.
  */
object Exercise18 {


  /** A parser is a kind of state action that can fail. */
  type Parser[+A] = ParseState => Result[A]

  trait InParser extends Parsers[Parser] {
    override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = {
      s =>
        p1(s) match {
          case Failure(e, false) => p2(s).mapError(_.addFailure(e))
          case r => r // committed failure or success skips running `p2`
        }
    }

  }

  sealed trait Result[+A] {

    /* Used by `scope`, `label`. */
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }
  }

  case class Success[+A](get: A, length: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]


  case class ParseError(stack: List[(Location,String)] = List(),
                        otherFailures: List[ParseError] = List()) extends ParsingError {

    def addFailure(e: ParseError): ParseError =
      this.copy(otherFailures = e :: this.otherFailures)

  }

  // we have to decide how to print a `ParseError` for human consumption We also can expose combinators for selecting
  // which error(s) get reported in the event that a chain of `a | b | c` fails--we might choose to collect up all the
  // errors for each of the three parsers, or perhaps only show the parser that got the furthest in the input before
  // failing, etc

}
