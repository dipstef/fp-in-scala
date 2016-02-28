package chapter07.exercises

import java.util.concurrent.{Executors, TimeUnit}

import chapter07.Par.{Par, asyncF, map, sequence}

/**
  * Implement parFilter, which filters elements of a list in parallel.
  */
object Exercise06 {

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF((a: A) => if (f(a)) List(a) else List())
    // convenience method on `List` for concatenating a list of lists
    map(sequence(pars))(_.flatten)
  }

  // alternate version
  def parFilter_[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[(A, Boolean)]] = l map asyncF(a => (a, f(a)))

    map(sequence(pars))(_.filter(_._2).map(_._1))

  }

  def main(args: Array[String]) {

    val l = (1 to 10).toList

    printResult(parFilter(l)(_ % 2 == 0))
    printResult(parFilter_(l)(_ % 2 == 0))
  }

  private def printResult(par: Par[List[Int]]): Unit = {
    val es = Executors.newCachedThreadPool()

    try {
      val result = par(es).get(2, TimeUnit.SECONDS)

      println("result = " + result)
    } finally {
      es.shutdown()
    }
  }
}
