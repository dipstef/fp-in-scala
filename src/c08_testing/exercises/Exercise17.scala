package c08_testing.exercises

import c07_parallelism.Par
import c08_testing.PropParallel
import c08_testing.PropParallel.equal

import c08_testing.exercises.Exercise16.pint2

/**
  * Express the property about fork from chapter 7, that fork(x) == x.”
  */
object Exercise17 {
  val forkProp = PropParallel.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"
}
