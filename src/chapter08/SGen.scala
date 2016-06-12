package chapter08

/*
  We’d like our framework to find the smallest or simplest failing test case, to better illustrate the problem and
  facilitate debugging:

  There are two general approaches:

  * Shrinking: After we’ve found a failing test case, we can run a separate procedure to minimize the test case by
    successively decreasing its “size” until it no longer fails.
    This is called shrinking, and it usually requires us to write separate code for each data type to implement
    this minimization process.

  * Sized generation: Rather than shrinking test cases after the fact, we simply generate our test cases in order of
    increasing size and complexity. So we start small and increase the size until we find a failure.
    This idea can be extended in various ways to allow the test runner to make larger jumps in the space of possible
    sizes while still making it possible to find the smallest failing test.


  We’ll see what we can do with sized generation. It’s a bit simpler and in some ways more modular, because our
  generators only need to know how to generate a test case of a given size.
  They don’t need to be aware of the “schedule” used to search the space of test cases, and the function that runs the
  tests therefore has the freedom to choose this schedule”


  */
case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen {
      g(_) map f
    }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap {
        f(_).g(n)
      }
    }
    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}

