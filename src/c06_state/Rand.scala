package c06_state

object Rand {
  type Rand[A] = State[RNG, A]
}
