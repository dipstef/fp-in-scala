package chapter11

import chapter06.State


/*
  State is short for computation that carries some state along, or state action, state transition, or even statement

  case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => St[S, B]): St[S, B] = St(
    state => {
      val (a, state1) = run(state)
      f(a).run(state1)
    }
  )

}

    It looks like State definitely fits the profile for being a monad. But its type constructor takes two type arguments,
    and Monad requires a type constructor of one argument, so we can’t just say Monad[State]. But if we choose some
    particular S, then we have something like State[S, _], which is the kind of thing expected by Monad.

    So State doesn’t just have one monad instance but a whole family of them, one for each choice of S. We’d like to be
    able to partially apply State to where the S type argument is fixed to be some concrete type.
 */

// But we don't have to create a full class like `StateMonads`. We can create an anonymous class inline, inside
// parentheses, and project out its type member, `lambda`:

trait StateMonad[S] extends Monad[({type lambda[x] = State[S, x]})#lambda] {

  def unit[A](a: => A): State[S, A] = State(s => (a, s))

  override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f

  // gets the current state
  def getState: State[S, S] = State(s => (s, s))

  // sets a new state
  def setState(s: S): State[S, Unit] = State(_ => ((), s))

}


object StateMonad {

  def stateMonad[S] = new StateMonad[S]{}

  // gets the current state
  def getState[S]: State[S, S] = stateMonad[S].getState

  // sets a new state
  def setState[S](s: S): State[S, Unit] = stateMonad[S].setState(s)


  def main(args: Array[String]): Unit = {
    val F = stateMonad[Int]

    /* This function numbers all the elements in a list using a State action. It keeps a state that’s an Int, which is
      incremented at each step. We run the whole composite state action starting from 0. We then reverse the result
      since we constructed it in reverse order.
     */

    def zipWithIndex[A](as: List[A]): List[(Int,A)] =
      as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) => for {
        xs <- acc
        n <- getState
        _ <- setState(n + 1)
      } yield (n, a) :: xs).run(0)._1.reverse

    println(s"zipWithIndex(List(1,2,3,4)) = ${zipWithIndex(List(1, 2, 3, 4))}")
  }

}
