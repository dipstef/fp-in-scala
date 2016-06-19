package chapter02.exercises

/**
  * Implement the higher-order function that composes two functions.
  */
object Exercise05 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
