package c08_testing.exaustive

import c05_laziness.{Cons, Stream}
import c06_state.{RNG, State}


/*
This source file contains the answers to the last two exercises in the section
"Test Case Minimization" of chapter 8 on property-based testing.

The Gen data type in this file incorporates exhaustive checking of finite domains.
*/

import c08_testing.exaustive.Gen._
import c08_testing.exaustive.Prop._


/*
The `Gen` type now has a random generator as well as an exhaustive stream.
Infinite domains will simply generate infinite streams of None.
A finite domain is exhausted when the stream reaches empty.
*/
case class Gen[+A](sample: State[RNG, A], exhaustive: Stream[Option[A]]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f),
      map2Stream(exhaustive, g.exhaustive)(map2Option(_, _)(f)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample),
      exhaustive.flatMap {
        case None => unbounded
        case Some(a) => f(a).exhaustive
      })

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def unsized = Unsized(this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))
}

object Gen {
  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a), bounded(Stream(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean), bounded(Stream(true, false)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)),
      bounded(Stream.from(start).take(stopExclusive - start)))

  /* This implementation is rather tricky, but almost impossible to get wrong
   * if you follow the types. It relies on several helper functions (see below).
   */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)),
      cartesian(Stream.constant(g.exhaustive).take(n)).
        map(l => sequenceOption(l.toList)))

  /* `cartesian` generates all possible combinations of a `Stream[Stream[A]]`. For instance:
   *
   *    cartesian(Stream(Stream(1,2), Stream(3), Stream(4,5))) ==
   *    Stream(Stream(1,3,4), Stream(1,3,5), Stream(2,3,4), Stream(2,3,5))
  */
  def cartesian[A](s: Stream[Stream[A]]): Stream[Stream[A]] =
    s.foldRight(Stream(Stream[A]()))((hs, ts) => map2Stream(hs, ts)(Stream.cons(_, _)))

  /* `map2Option` and `map2Stream`. Notice the duplication! */
  def map2Option[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {a <- oa; b <- ob} yield f(a, b)

  /* This is not the same as `zipWith`, a function we've implemented before.
   * We are generating all (A,B) combinations and using each to produce a `C`.
   * This implementation desugars to sa.flatMap(a => sb.map(b => f(a,b))).
   */
  def map2Stream[A, B, C](sa: Stream[A], sb: => Stream[B])(f: (A, => B) => C): Stream[C] =
    for {a <- sa; b <- sb} yield f(a, b)

  /* This is a function we've implemented before. Unfortunately, it does not
   * exist in the standard library. This implementation is uses a foldLeft,
   * followed by a reverse, which is equivalent to a foldRight, but does not
   * use any stack space.
   */
  def sequenceOption[A](o: List[Option[A]]): Option[List[A]] =
    o.foldLeft[Option[List[A]]](Some(List()))(
      (t, h) => map2Option(h, t)(_ :: _)).map(_.reverse)

  /* Notice we are using the `unbounded` definition here, which is just
   * `Stream(None)` in our current representation of `exhaustive`.
   */
  def uniform: Gen[Double] =
    Gen(State(RNG.double), unbounded)

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d * (j - i)), unbounded)

  /* Basic idea is add 1 to the result of `choose` if it is of the wrong
   * parity, but we require some special handling to deal with the maximum
   * integer in the range.
   */
  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 != 0) n + 1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 != 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 == 0) n + 1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for {
    i <- choose(from, to)
    j <- if (i % 2 == 0) even(from, to) else odd(from, to)
  } yield (i, j)

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((a, b) => a.map2(b)(_ :: _))

  /* The simplest possible implementation. This will put all elements of one
   * `Gen` before the other in the exhaustive traversal. It might be nice to
   * interleave the two streams, so we get a more representative sample if we
   * don't get to examine the entire exhaustive stream.
   */
  def union_1[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen(
      State(RNG.boolean).flatMap(b => if (b) g1.sample else g2.sample),
      interleave(g1.exhaustive, g2.exhaustive)
    )

  def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] =
    s1.zipAll(s2).flatMap { case (a, a2) => Stream(a.toList ++ a2.toList: _*) }

  /* The random case is simple - we generate a double and use this to choose between
   * the two random samplers. The exhaustive case is trickier if we want to try
   * to produce a stream that does a weighted interleave of the two exhaustive streams.
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    /* Some random booleans to use for selecting between g1 and g2 in the exhaustive case.
     * Making up a seed locally is fine here, since we just want a deterministic schedule
     * with the right distribution. */
    def bools: Stream[Boolean] =
      randomStream(uniform.map(_ < g1Threshold))(RNG.Simple(302837L))

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample),
      interleave(bools, g1._1.exhaustive, g2._1.exhaustive))
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  /* Interleave the two streams, using `b` to control which stream to pull from at each step.
   * A value of `true` attempts to pull from `s1`; `false` attempts to pull from `s1`.
   * When either stream is exhausted, insert all remaining elements from the other stream.
   */
  def interleave[A](b: Stream[Boolean], s1: Stream[A], s2: Stream[A]): Stream[A] =
    b.headOption map { hd =>
      if (hd) s1 match {
        case Cons(h, t) => Stream.cons(h(), interleave(b drop 1, t(), s2))
        case _ => s2
      }
      else s2 match {
        case Cons(h, t) => Stream.cons(h(), interleave(b drop 1, s1, t()))
        case _ => s1
      }
    } getOrElse Stream.empty

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    Sized(n => g.listOfN(n))

  /* Not the most efficient implementation, but it's simple.
   * This generates ASCII strings.
   */
  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  def string: SGen[String] = Sized(stringN)

  case class Sized[+A](forSize: Int => Gen[A]) extends SGen[A]

  case class Unsized[+A](get: Gen[A]) extends SGen[A]

  implicit def unsized[A](g: Gen[A]): SGen[A] = Unsized(g)

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    Sized(n => g.listOfN(n max 1))


  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }


  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => s => i)
}


object Values {

  val smallInt: Gen[MaxSize] = Gen.choose(-10, 10)
  val maxProp: Prop = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }


  val maxProp1: Prop = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  val sortedProp: Prop = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !l.zip(ls.tail).exists { case (a, b) => a > b }
  }
}