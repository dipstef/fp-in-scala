package chapter07.exercises

/**
  * Hard: Given map(y)(id) == y, it’s a free theorem that map(map(y)(g))(f) == map(y)(f compose g)
  *
  * is sometimes called map fusion, and it can be used as an optimization—rather than spawning a separate parallel
  * computation to compute the second mapping, we can fold it into the first mapping.)
  *
  * Can you prove it? You may want to read the paper “Theorems for Free!” (http://mng.bz/Z9f1) to better understand
  * the “trick” of free theorems.
  *
  * Our representation of Par doesn’t give us the ability to implement this optimization, since it’s an opaque function.
  * If it were reified as a data type, we could pattern match and discover opportunities to apply this rule.
  * You may want to try experimenting with this idea on your own.
 */
object Exercise07 {

}
