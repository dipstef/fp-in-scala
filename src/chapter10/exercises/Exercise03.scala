package chapter10.exercises

import chapter10.Monoid

/**
  * A function having the same argument and return type is sometimes called an endofunction.[2] Write a monoid for endofunctions.
  *
  * 2 The Greek prefix endo- means within, in the sense that an endofunction’s codomain is within its domain.
  */
object Exercise03 {

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

    def op(f: A => A, g: A => A): (A) => A = f compose g

    val zero: (A) => A = (a: A) => a
  }


}
