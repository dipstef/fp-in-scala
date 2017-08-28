package c02_fun.exercises

/**
  * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function
  */
object Exercise02 {

  def isSorted[A](as: Array[A], firstGreaterThanSecond: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def check(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else if (firstGreaterThanSecond(as(i), as(i + 1))) false
      else check(i + 1)
    }
    check(0)
  }
}
