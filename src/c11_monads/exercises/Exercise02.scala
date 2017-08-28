package c11_monads.exercises

import c06_state.State
import c11_monads.Monad

/**
  * Hard: State looks like it would be a monad too, but it takes two type arguments and you need a type constructor of
  * one argument to implement Monad. Try to implement a State monad, see what issues you run into, and think about
  * possible solutions.
  *
  * We’ll discuss the solution later in this chapter.
  */
object Exercise02 {

  // Since `State` is a binary type constructor, we need to partially apply it with the `S` type argument. Thus, it is
  // not just one monad, but an entire family of monads, one for each type `S`.
  //
  // One solution is to create a class `StateMonads` that accepts the `S` type argument and then has a _type member_
  // for the fully applied `State[S, A]` type inside:
  class StateMonads[S] {
    type StateS[A] = State[S, A]

    // We can then declare the monad for the `StateS` type constructor:
    val monad = new Monad[StateS] {

      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
    }
  }
  // But we don't have to create a full class like `StateMonads`. We can create an anonymous class inline, inside
  // parentheses, and project out its type member, `lambda`:
  trait StateMonad[S] extends  Monad[({type lambda[x] = State[S, x]})#lambda] {

    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f

  }

  def stateMonad[S] = new StateMonads[S]{}

}
