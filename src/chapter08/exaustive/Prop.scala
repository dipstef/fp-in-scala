package chapter08.exaustive

import java.util.concurrent.Executors

import chapter05.{Cons, Stream}
import chapter06.RNG
import chapter07.Par
import chapter07.Par._
import chapter08.exaustive.Gen.{**, Sized, Unsized}
import chapter08.exaustive.Prop._
import chapter08.exaustive.Gen.{choose, randomStream, unit, weighted}
import chapter08.exaustive.Status._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Right((a, n_)) => p.run(max, n_, rng).right.map { case (s, m) => (s, n_ + m) }
      case l => l
    }
  }


  def ||(p: Prop) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Left(msg) => p.tag(msg).run(max, n, rng)
      case r => r
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Left(e) => Left(msg + "\n" + e)
      case r => r
    }
  }
}

object Prop {
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String
  type Result = Either[FailedCase, (Status, TestCases)]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => {
      def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result =
        if (i == j) Right((Unfalsified, i))
        else s match {
          case Cons(h, t) => h() match {
            case Some(h_) =>
              try {
                if (f(h_)) go(i + 1, j, t(), onEnd)
                else Left(h_.toString)
              }
              catch {
                case e: Exception => Left(buildMsg(h_, e))
              }
            case None => Right((Unfalsified, i))
          }
          case _ => onEnd(i)
        }
      go(0, n / 3, a.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(a)(rng).map(Some(_))
          go(n / 3, n, rands, i => Right((Unfalsified, i)))
        case s => s // If proven or failed, stop immediately
      }
    }
  }

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  /* We pattern match on the `SGen`, and delegate to our `Gen` version of `forAll`
   * if `g` is unsized; otherwise, we call the sized version of `forAll` (below).
   */
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match {
    case Unsized(g2) => forAll(g2)(f)
    case Sized(gs) => forAll(gs)(f)
  }

  /* The sized case of `forAll` is as before, though we convert from `Proven` to
   * `Exhausted`. A sized generator can never be proven, since there are always
   * larger-sized tests that were not run which may have failed.
   */
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = n / max + 1
      val props: List[Prop] =
        Stream.from(0).take(max + 1).map(i => forAll(g(i))(f)).toList
      val p: Prop = props.map(p => Prop((max, n, rng) => p.run(max, casesPerSize, rng))).
        reduceLeft(_ && _)
      p.run(max, n, rng).right.map {
        case (Proven, m) => (Exhausted, m)
        case x => x
      }
  }

  def run(p: Prop,
          maxSize: Int = 100, // A default argument of `200`
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Left(msg) => println("! test failed:\n" + msg)
      case Right((Unfalsified, n)) =>
        println("+ property unfalsified, ran " + n + " tests")
      case Right((Proven, n)) =>
        println("+ property proven, ran " + n + " tests")
      case Right((Exhausted, n)) =>
        println("+ property unfalsified up to max size, ran " +
          n + " tests")
    }
  }

  def check(p: => Boolean): Prop = // Note that we are non-strict here
    forAll(unit(()))(_ => p)


  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)


  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }


}