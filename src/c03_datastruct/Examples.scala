package c03_datastruct

object Lists {
  def increment(l: List[Int]): List[Int] = List.flatMap(l) { i => List(i + 1) }
}

object Examples extends App {

  assert(Lists.increment(List(1, 2, 3)) == List(2, 3, 4))

}
