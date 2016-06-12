package chapter08

import chapter06.{RNG, State}

case class Gen[+A](sample: State[RNG,A]) {
}
