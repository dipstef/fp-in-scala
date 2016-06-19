package chapter03.exercises

/**
  * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
  * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
  *
  * This is a deeper question that we’ll return to in chapter 5.
  */
object Exercise07 {

  /*
   this is not possible. The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
   which in the case of `foldRight` means traversing the list all the way to the end.

   We need _non-strict_ evaluation to support early termination.
  */

}
