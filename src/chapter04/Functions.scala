package chapter04

import chapter04.exercises.Exercise02

object Functions {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = Exercise02.variance(xs)

}
