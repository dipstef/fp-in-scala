package c06_state.exercises

import c06_state.State
import c06_state.State.{get, modify, sequence}

/**
  * Hard: To gain experience with the use of State, implement a finite state automaton that models a simple candy
  * dispenser. The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
  * It can be in one of two states: locked or unlocked. It also tracks how many candies are left and how many coins it
  * contains
  *
  *
  * The rules of the machine are as follows:
  *
  * Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
  * Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
  * Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
  *
  * A machine that’s out of candy ignores all inputs.
  *
  * The method simulateMachine should operate the machine based on the list of inputs and return the number of coins
  * and candies left in the machine at the end. For example, if the input Machine has 10 coins and 5 candies, and a
  * total of 4 candies are successfully bought, the output should be (14, 1).
  */
object Exercise11 {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object CandyMachine {

    def update = (i: Input) => (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(false, _, _)) => s
      }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
  }

  import CandyMachine._

  def main(args: Array[String]) {

    assert(update(Coin)(Machine(locked = true, candies = 12, coins = 0)) ==
                        Machine(locked = false, candies = 12, coins = 1))

    assert(update(Turn)(Machine(locked = false, candies = 12, coins= 1)) ==
                        Machine(locked=true, candies = 11, coins = 1))


  }


}
