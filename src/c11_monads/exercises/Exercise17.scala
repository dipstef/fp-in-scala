package c11_monads.exercises

import c11_monads.Monad

/**
  * Implement map and flatMap as methods on this class, and give an implementation for Monad[Id].
  */
object Exercise17 {

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }


  val idMonad: Monad[Id] = new Monad[Id] {

    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)
  }

  def main(args: Array[String]): Unit = {
    val id1 = Id("Hello, ") flatMap (a => Id("monad!") flatMap (b => Id(a + b)))
    val id2 = Id("Hello, ") flatMap (a => Id("monad!") map (b => a + b))

    val id3 = for {a <- Id("Hello, ");b <- Id("monad!")} yield a + b

    println(s"id1 = $id1")
    println(s"id2 = $id2")
    println(s"id3 = $id3")

    /*
      So what is the action of flatMap for the identity monad? It’s simply variable substitution.

      The variables a and b get bound to "Hello, " and "monad!", respectively, and then substituted into the expression
      a + b. We could have written the same thing without the Id wrapper, using just Scala’s own variables:

      scala> val a = "Hello, "
      a: java.lang.String = "Hello, "

      scala> val b = "monad!"
      b: java.lang.String = monad!

      scala> a + b
      res2: java.lang.String = Hello, monad!

      Besides the Id wrapper, there’s no difference. So now we have at least a partial answer to the question of what
      monads mean. We could say that monads provide a context for introducing and binding variables, and performing
      variable substitution.

     */
  }


}
