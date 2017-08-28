package c06_state.exercises

import c06_state.{State, RNG}

/**
  * Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods on the State case class where
  * possible. Otherwise you should put them in a State companion object
  */
object Exercise10 {

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  }


  import State._

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(state => {
      val (a, state1) = run(state)
      f(a).run(state1)
    })

  }

}
