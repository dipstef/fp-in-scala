package chapter06


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  case class Simple(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = Simple(newSeed)
      // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)
    }
  }

  // Exercise 01
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    // `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
    (if (i < 0) -(i + 1) else i, r)
  }

  // Exercise 02
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Integer.MAX_VALUE.toDouble + 1), r)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 03
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }
  // Exercise 03
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d, i), r)
  }
  // Exercise 03
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  // Exercise 04
  def _ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      count match {
        case x if x <= 0 => (xs, r)
        case _ =>
          val (x, r2) = r.nextInt
          go(count -1, r2, x::xs)
      }
    go(count, rng, List())
  }


  // The Form (A, RNG) is called state action. These state actions can be combined using combinators.
  // We want to write combinators that let us combine Rand actions while avoiding explicitly passing along the RNG state.
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // a simple RNG state transition is the unit action, which passes the RNG state through without using it,
  // always returning a constant value rather than a random value”
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // transforming the output of a state action without modifying the state itself.
  // Rand[A] is  a type alias for a function type RNG => (A, RNG), so this is just a kind of function composition
  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Example on how to use map
  def nonNegativeEven: Rand[Int] = _map(nonNegativeInt)(i => i - i % 2)

  // Exercise 05
  def _double: Rand[Double] = map(nonNegativeInt)(i => i / (Integer.MAX_VALUE.toDouble + 1))

  // Unfortunately, map isn’t powerful enough to implement intDouble and doubleInt from exercise 3.
  // What we need is a new combinator map2 that can combine two RNG actions into one using a binary rather than unary
  // function
  // Exercise 06
  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  // We only have to write the map2 combinator once, and then we can use it to combine arbitrary RNG state actions

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = _map2(ra, rb)((_, _))

  // We can use this to reimplement intDouble and doubleInt from exercise 6.3 more succinctly

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 07
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // Exercise 07
  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Exercise 08
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  // Exercise 08
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // Exercise 09
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  // Exercise 09
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

}
