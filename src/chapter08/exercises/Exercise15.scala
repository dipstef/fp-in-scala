package chapter08.exercises

/**
  * A check property is easy to prove conclusively because the test just involves evaluating the Boolean argument.
  *But some forAll properties can be proved as well. For instance, if the domain of the property is Boolean, then there
  *are really only two cases to test.
  **
  * If a property forAll(p) passes for both p(true) and p(false), then it is proved. Some domains (like Boolean and Byte)
  *are so small that they can be exhaustively checked. And with sized generators, even infinite domains can be exhaustively
  *checked up to the maximum size. Automated testing is very useful, but it’s even better if we can automatically prove
  *our code correct.
  **
  * Modify our library to incorporate this kind of exhaustive checking of finite domains and sized generators
  *
  */
object Exercise15 {

}
