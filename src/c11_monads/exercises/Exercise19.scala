package c11_monads.exercises

import c06_state.State
import c11_monads.StateMonad

/**
  * What does this tell us about the meaning of the State monad? Let’s study a simple example. The details of this code
  * aren’t too important, but notice the use of getState and setState in the for block.
  */
object Exercise19 {

  trait This[S] extends StateMonad[S] {

    def gettingAndSettingSameState: State[S, Unit] = {
      // Getting and setting the same state does nothing:
      getState.flatMap(setState) == unit(())

      // written as for-comprehension:
      for {
        x <- getState
        _ <- setState(x)
      } yield ()
    }

    def settingAndGetting(s: S) {
      // Setting the state to `s` and getting it back out yields `s`.
      setState(s).flatMap(_ => getState) == unit(s)

      // alternatively:
      for {
        _ <- setState(s)
        x <- getState
      } yield x
    }
  }

}
