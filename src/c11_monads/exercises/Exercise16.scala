package c11_monads.exercises

object Exercise16 {

  /*
    Recall the identity laws:

    left identity:  flatMap(unit(x))(f) == f(x)
    right identity: flatMap(x)(unit)    == x

    The left identity law for `Gen`:
    The law states that if you take the values generated by `unit(x)` (which are always `x`) and apply `f` to those
    values, that's exactly the same as the generator returned by `f(x)`.

    The right identity law for `Gen`:
    The law states that if you apply `unit` to the values inside the generator `x`, that does not in any way differ from `x` itself.

    The left identity law for `List`:
    The law says that wrapping a list in a singleton `List` and then flattening the result is the same as doing nothing.

    The right identity law for `List`:
    The law says that if you take every value in a list, wrap each one in a singleton `List`, and then flatten the result,
    you get the list you started with.

   */

}