package chapter08.exaustive

sealed trait Status {}

object Status {
  case object Exhausted extends Status
  case object Proven extends Status
  case object Unfalsified extends Status
}
