package chapter04.exercises

import chapter04.Option
import chapter04.Functions.mean

/**
  * Implement the variance function in terms of flatMap.
  *
  * If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  */
object Exercise02 {

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

}
