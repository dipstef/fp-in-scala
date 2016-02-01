package chapter06

object Rand {
  type Rand[A] = State[RNG, A]
}
