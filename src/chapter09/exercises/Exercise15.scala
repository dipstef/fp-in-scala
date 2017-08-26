package chapter09.exercises

/**
  * Implement the rest of the primitives, including run, using this representation of Parser, and try running your
  * JSON parser on various inputs.
  *
  * You’ll find, unfortunately, that it causes stack overflow for large inputs (for instance, [1,2,3,...10000]).
  *
  * One simple solution to this is to provide a specialized implementation of many that avoids using a stack frame for
  * each element of the list being built up. So long as any combinators that do repetition are defined in terms of
  * many (which they all can be), this solves the problem. See the answers for discussion of more general approaches.
  */
object Exercise15 {

  // See Reference.scala

}
