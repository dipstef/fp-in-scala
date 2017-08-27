package chapter11.exercises

import chapter11.Monad


/**
  * Hard: Implement flatMap in terms of compose. It seems that we’ve found another minimal set of monad combinators:
  * compose and unit.
  */
object Exercise08 {


  trait This[M[_]] extends Monad[M] {
    override def _flatMap[A, B](ma: M[A])(f: (A) => M[B]): M[B] =
      // compose((_:M[A]) => ma, f)(ma)
      // compose((_:Any) => ma, f)(ma)
      compose((_: Unit) => ma, f)()
  }


  def main(args: Array[String]): Unit = {

    val listMonad = new This[List] {

      override def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)
    }

    println(s"result = ${listMonad._flatMap(List(1, 2, 3, 4, 5, 6))(a => List(a + 1))}")
  }

}