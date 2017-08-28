package c09_parsing.exercises

import c09_parsing.Parsers

/**
  * We could also deal with non-strictness with a separate combinator like we did in chapter 7. Try this here and make
  * the necessary changes to your existing combinators. What do you think of that approach in this instance?
  */
object Exercise05 {

  trait This[Parser[+ _]] extends Parsers[Parser] {

    // strict on the second parameter
    def map2_[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = map(product(p, p2))(f.tupled)

    // We could introduce a combinator, `wrap`:
    def wrap[A](p: => Parser[A]): Parser[A]

    def many_[A](p: Parser[A]): Parser[List[A]] = map2(p, wrap(many(p)))(_ :: _) or succeed(List())

    // In the parallelism chapter, we were particularly interested in avoiding having `Par` objects that took as much
    // time and space to build as the corresponding serial computation, and the `delay` combinator let us control
    // this more carefully.
    //
    // Here, this isn't as much of a concern, and having to think carefully each time we `map2` to decide whether we
    // need to call `wrap` seems like unnecessary friction for users of the API.

  }
}
