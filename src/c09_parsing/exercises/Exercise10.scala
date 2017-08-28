package c09_parsing.exercises

/**
  * Hard: If you haven’t already done so, spend some time discovering a nice set of combinators for expressing what
  * errors get reported by a Parser.
  *
  * For each combinator, try to come up with laws specifying what its behavior should be. This is a very open-ended
  * design task.
  *
  * Here are some guiding questions:
  *
  * Given the parser "abra".**(" ".many).**("cadabra"), what sort of error would you like to report given the input
  * "abra cAdabra" (note the capital 'A')?
  *
  * Only something like Expected 'a'? Or Expected "cadabra"? What if you wanted to choose a different error message,
  * like "Magic word incorrect, try again!"?
  *
  * Given a or b, if a fails on the input, do we always want to run b, or are there cases where we might not want to?
  * If there are such cases, can you think of additional combinators that would allow the programmer to specify when
  * or should consider the second parser?
  *
  * How do you want to handle reporting the location of errors?
  * Given a or b, if a and b both fail on the input, might we want to support reporting both errors? And do we always
  * want to report both errors, or do we want to[…]
  */
object Exercise10 {

}
