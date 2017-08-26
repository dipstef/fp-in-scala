package chapter09

import java.util.regex.Pattern

import chapter08.{Gen, Prop}

import scala.util.matching.Regex
import language.higherKinds
import language.implicitConversions


trait BaseParsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParsingError, A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  // Chooses between two parsers, first attempting p1, and then p2 if p1 fails
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  // We’ll assume that or is left-biased, meaning it tries p1 on the input, and then tries p2 only if p1 fails

  // Always succeeds with the value a
  def succeed[A](a: A): Parser[A]

  // flatMap enables context-sensitive parsers by allowing the selection of a second parser to depend on the result of
  // the first parser.
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // We call this combinator slice since we intend for it to return the portion of the input string examined by the
  // parser if successful
  def slice[A](p: Parser[A]): Parser[String]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {a <- p; b <- p2} yield f(a, b)

  // If map2 would be strict on the second argument, our many function will never terminate!
  // This indicates that we need to make product and map2 non-strict in their second argument:

  // Conceptually, product should have been non-strict in its second argument anyway, since if the first Parser fails,
  // the second won’t even be consulted.
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  // What if we want to recognize one or more 'a' characters”
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  // We need some way of running one parser, followed by another, assuming the first is successful.
  // These can be implemented using a for-comprehension, which delegates to the `flatMap` and `map` implementations we've
  // provided on `ParserOps`, or they can be implemented in terms of these functions directly.

  // Sequences two parsers, running p1 and then p2, and returns the pair of their results if both succeed
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    // flatMap(p)(a => map(p2)(b => (a, b)))
    for {a <- p; b <- p2} yield (a, b)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)


  implicit def regex(r: Regex): Parser[String]


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    // use `self` to explicitly disambiguate reference to the `or` method on the `trait`
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
  }


}

trait Parsers[Parser[+_]] extends BaseParsers[Parser] {
  self =>

  implicit def extendedOperators[A](p: Parser[A]): ExtendedParserOps[A] = ExtendedParserOps[A](p)

  implicit def asExtendedParser[A](a: A)(implicit f: A => Parser[String]): ExtendedParserOps[String] = ExtendedParserOps(f(a))

  /** delays committing to a parse:
    * if p fails midway through examining the input, attempt reverts the commit to that parse
    * (and can allows another parse to run in a or expression)
    *
    * The attempt combinator can be used whenever there’s ambiguity in the grammar and multiple tokens may have to be
    * examined before the ambiguity can be resolved and parsing can commit to a single branch
    * */
  def attempt[A](p: Parser[A]): Parser[A]

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] = map2(slice(p), p2)((_,b) => b)

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] = map2(p, slice(p2))((a,b) => a)

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] = attempt(p) <* whitespace

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
  // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)


  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]): Parser[A] = start *> p <* stop

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] = regex("\\z".r).label("unexpected trailing characters")


  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] = p <* eof

  /** Parses a sequence of left-associative binary operators with the same precedence. */
  def opL[A](p: Parser[A])(op: Parser[(A,A) => A]): Parser[A] = map2(p, many(op ** p))((h,t) => t.foldLeft(h)((a,b) => b._1(a,b._2)))

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  // a way to nest labels
  // scope doesn’t throw away the label(s) attached to p—it merely adds additional information in the event that p fails
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
  // rather annoying to write, left as an exercise
  // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] = doubleString map (_.toDouble) label "double literal"

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] = token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)


  case class ExtendedParserOps[A](p: Parser[A]) {
    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)
    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)
    def token: Parser[A] = self.token(p)
    def sep(separator: Parser[Any]): Parser[List[A]] = self.sep(p, separator)
    def sep1(separator: Parser[Any]): Parser[List[A]] = self.sep1(p, separator)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
    def opL(op: Parser[(A,A) => A]): Parser[A] = self.opL(p)(op)

    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)

  }
}

trait ParsingError