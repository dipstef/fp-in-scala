package c11_monads.exercises

import c11_monads.Monad

/**
  * Hard: Here’s an example of a function we haven’t seen before. Implement the function filterM. It’s a bit like filter,
  * except that instead of a function from A => Boolean, we have an A => F[Boolean].
  *
  * (Replacing various ordinary functions like this with the monadic equivalent often yields interesting results.)
  * Implement this function, and then think about what it means for various data types.
  */
object Exercise06 {

  /*
    For `Par`, `filterM` filters a list, applying the functions in parallel; for `Option`, it filters a list, but allows
    the filtering function to fail and abort the filter computation; for `Gen`, it produces a generator for  subsets of
    the input list, where the function `f` picks a 'weight' for each element (in the form of a `Gen[Boolean]`)
  */

  trait This[M[_]] extends Monad[M] {

    override def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
      case Nil => unit(List[A]())
      case x :: xs => map2(f(x), filterM(xs)(f)) { (p, fxs) => if (p) x :: fxs else fxs }
    }

    def filterM_[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
      ms match {
        case Nil => unit(Nil)
        case h :: t => flatMap(f(h))(b =>
          if (!b) filterM_(t)(f)
          else map(filterM_(t)(f))(h :: _))
      }


  }

  def main(args: Array[String]): Unit = {

    val listMonad = new This[List] {

      override def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)
    }

    val result = listMonad.filterM(List(1,2,3,4,5,6))(a => List(a % 2 == 0))

    println(s"result = $result")
  }

}
