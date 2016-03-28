package chapter07.examples
import java.util.concurrent.Executors

import chapter07.ParNb.{parMap, run}

object SqrtTest {
  def main(args: Array[String]) {
    val p = parMap(List.range(1, 100000))(math.sqrt(_))

    val x = run(Executors.newFixedThreadPool(2))(p)

    println("x = " + x)
  }

}