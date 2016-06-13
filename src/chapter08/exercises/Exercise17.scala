package chapter08.exercises

import chapter07.Par
import chapter08.PropParallel
import chapter08.PropParallel.equal

import chapter08.exercises.Exercise16.pint2

/**
  * Express the property about fork from chapter 7, that fork(x) == x.”
  */
object Exercise17 {
  val forkProp = PropParallel.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"
}
