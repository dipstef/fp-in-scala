package chapter03.exercises

import chapter03.{Cons, List, Nil}
import chapter03.List.foldRight

/**
  * See what happens when you pass Nil and Cons themselves to foldRight, like this:
  *
  * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
  *
  * What do you think this says about the relationship between foldRight and the data constructors of List?
  *
  * (The type annotation Nil:List[Int] is needed here, because otherwise Scala infers the B type parameter in foldRight
  * as List[Nothing])
  */
object Exercise08 {

  /*
    We get back the original list! Why is that? As we mentioned earlier, one way of thinking about what `foldRight` "does" is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list.

    foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
    Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
    Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
    Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
    Cons(1, Cons(2, Cons(3, Nil)))
  */

  def main(args: Array[String]) {
    val l1 = List(1, 2, 3)

    val l2 = foldRight(l1, Nil: List[Int])(Cons(_, _))

    println("l2 = " + l2)

  }

}
