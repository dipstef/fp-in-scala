package chapter08.exercises

/**
  * *
  * Assuming the following representation of Prop, implement && as a method of Prop.
  * *
  * trait Prop { def check: Boolean }
  *
  */
object Exercise03 {

  /* Since check has a side effect, the only option for implementing && in this case would be to run both check methods.
     So if check prints out a test report, then we would get two of them, and they would print failures and successes
     independently of each other.
     That’s likely not a correct implementation. The problem is not so much that check has a side effect,
     but more generally that it throws away information

     In order to combine Prop values using combinators like &&, we need check (or whatever function “runs” properties)
     to return some meaningful value. What type should that value have? Well, let’s consider what sort of information
     we’d like to get out of checking our properties. At a minimum, we need to know whether the property succeeded or
     failed. This lets us implement &&.

     */

  trait Prop {
    def check: Boolean

    def &&(p: Prop): Prop = new Prop {
      def check = Prop.this.check && p.check

    }
  }

}
