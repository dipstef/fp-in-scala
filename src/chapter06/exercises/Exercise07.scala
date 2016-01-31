package chapter06.exercises

import chapter06.RNG.{SimpleRNG, Rand, unit, map2, int}

/**
  * Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
  * Implement sequence for combining a List of transitions into a single transition.
  * Use it to reimplement the ints function you wrote before. For the latter, you can use the standard library
  * function List.fill(n)(x) to make a list with x repeated n times.
  */
object Exercise07 {
  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `f` is the current element in the list.
  // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  //
  // We are using `foldRight`. If we used `foldLeft` then the values in the
  // resulting list would appear in reverse order.

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def main(args: Array[String]) {
    val rng = new SimpleRNG(123)

    println("sequence(List(unit(1), unit(2), unit(3)))(rng) = " + sequence(List(unit(1), unit(2), unit(3)))(rng))

    assert(sequence(List(unit(1), unit(2), unit(3)))(rng) == (List(1,2,3), rng))
  }

}
