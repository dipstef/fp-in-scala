package chapter10.exercises

import chapter10.Monoid

/**
  * Give Monoid instances for integer addition and multiplication as well as the Boolean operators.
  */
object Exercise01 {

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x + y

    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x * y

    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x || y

    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x && y

    val zero = true
  }

}
