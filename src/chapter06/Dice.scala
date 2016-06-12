package chapter06

import chapter06.RNG.{Simple, Rand, map, nonNegativeLessThan}

object Dice {

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def main(args: Array[String]) {
    for( i <- 1 to 6) {
      println("nonNegativeLessThan(10)(rng) = " + rollDie(Simple(i))._1)
    }
  }
}
