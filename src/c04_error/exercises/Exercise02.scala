package c04_error.exercises

import c04_error.Option
import c04_error.Options.mean

/**
  * Implement the variance function in terms of flatMap.
  *
  * If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  */
object Exercise02 {

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

}
