package c07_parallelism.examples
import java.util.concurrent.Executors

import c07_parallelism.ParNb.{parMap, run}

object SqrtTest {
  def main(args: Array[String]) {
    val p = parMap(List.range(1, 100000))(math.sqrt(_))

    val x = run(Executors.newFixedThreadPool(2))(p)

    x.foreach(println)
  }

}