package chapter08.exercises

import chapter06.RNG

/**
  * Implement && and || for composing Prop values. Notice that in the case of failure we don’t know which property was
  * responsible, the left or the right.
  * Can you devise a way of handling this, perhaps by allowing Prop values to be assigned a tag or label which gets
  * displayed in the event of a failure?
  *
  */
object Exercise09 {

  import Prop._


  // needs to generate random test cases using our current representation of Gen
  case class Prop(run: (TestCases, RNG) => Result) {
    def &&(p: Prop) = Prop {
      (n, rng) => run(n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
    }

    def ||(p: Prop) = Prop {
      (n, rng) => run(n, rng) match {
        // In case of failure, run the other prop.
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
    }

    /* This is rather simplistic - in the event of failure, we simply prepend
     * the given message on a newline in front of the existing message.
     */
    def tag(msg: String) = Prop {
      (n, rng) => run(n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
    }
  }


  object Prop {
    type SuccessCount = Int
    type TestCases = Int
    type MaxSize = Int
    type FailedCase = String

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase,
                         successes: SuccessCount) extends Result {
      def isFalsified = true
    }

    case object Proved extends Result {
      def isFalsified = false
    }

  }
