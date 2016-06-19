package chapter02.exercises

/**
  * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
  *
  * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the previous two—the sequence
  * begins 0, 1, 1, 2, 3, 5. Your definition should use a local tail-recursive function.
  */
object Exercise01 {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def acc(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else acc(n - 1, cur, prev + cur)
    }
    acc(n, 0, 1)
  }
}
