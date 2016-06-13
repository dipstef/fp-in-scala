package chapter07.exercises

import chapter07.ParNb.{Par, Future, eval, map}

/**
  * Implement choiceN and then choice in terms of choiceN.
  */
object Exercise11 {

  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        p(es)(ind =>
          eval(es) {
            ps(ind)(es)(cb)
          }
        )
    }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))


}
