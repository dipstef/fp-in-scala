package chapter06

import chapter06.Rand.Rand


object ImperativeStyle {

  def ints(count: Int): Rand[List[Int]] = ???

  def flatMapChain(int: Rand[Int]) = {
    int.flatMap(x =>
      int.flatMap(y =>
        ints(x).map(xs =>
          xs.map(_ % y))))
  }


  def imperativeLike(int: Rand[Int]) = for {
    x <- int
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % y)


}
