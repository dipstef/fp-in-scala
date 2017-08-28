package c07_parallelism.exercises

import c07_parallelism.ParNb.{Future, Par}

/**
  * Implement this new primitive chooser, and then use it to implement choice and choiceN.
  */
object Exercise13 {

  /*
    chooser is perhaps no longer the most appropriate name for this operation, which is actually quite general—it’s a
    parallel computation that, when run, will run an initial computation whose result is used to determine a second computation.

    Nothing says that this second computation needs to even exist before the first computation’s result is available.
    It doesn’t need to be stored in a container like List or Map. Perhaps it’s being generated from whole cloth using
    the result of the first computation. This function, which comes up often in functional libraries, is usually called
    bind or flatMap
   */
  def chooser[A,B](p: Par[A])(choices: A => Par[B]): Par[B] = {
    flatMap(p)(choices)
  }

  /*
    flatMap is suggestive of the fact that this operation could be decomposed into two steps: mapping f: A => Par[B]
    over our Par[A], which generates a Par[Par[B]] and then flattening this nested Par[Par[B]] to a Par[B].

    But this is interesting, it suggests that all we needed to do was add an even simpler combinator, let’s call it join,
    for converting a Par[Par[X]] to Par[X] for any choice of X
   */
  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] = {
    es => new Future[B]{
      def apply(cb: B => Unit): Unit =
        p(es)(a => f(a)(es)(cb))
    }
  }

  def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    flatMap(p)(b => if (b) t else f)

  def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(p)(i => choices(i))

}
