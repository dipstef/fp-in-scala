package chapter11.exercises

import chapter11.Monad

/**
  * Prove that the identity laws hold for a monad of your choice.
  */
object Exercise11 {

  trait This extends Monad[Option] {

    def f[A]: (A) => A = identity[A]
    val v = 1

    // For `Option`, we again consider both cases `None` and `Some` and expand the equation.
    // The monadic `unit` is the `Some(_)` constructor.

    // Left identity is trivially true for None:
    flatMap(None)(Some(_)) == None

    // And here it is for Some:
    flatMap(Some(v))(Some(_)) == Some(v)
    // Substitute the definition of `flatMap`:
    Some(v) == Some(v)

    // Right identity is just as easy for None:
    flatMap(Some(None))(f) == f(None)
    // Substitute definition of flatMap:
    f(None) == f(None)

    // And for Some:
    flatMap(Some(Some(v)))(f) == f(Some(v))
    // Substitute definition of flatMap:
    f(Some(v)) == f(Some(v))

  }

}
