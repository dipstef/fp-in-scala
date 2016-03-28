package chapter07.exercises

import chapter07.ParNb.{Future, Par}

/**
  * There’s still something rather arbitrary about choiceN. The choice of List seems overly specific.
  * Why does it matter what sort of container we have? For instance, what if, instead of a list of computations,
  * we have a Map of them.
  */
object Exercise12 {

  def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] = {
    es => new Future[V] {
      def apply(cb: (V) => Unit): Unit = {
        p(es)(k => {
          ps(k)(es)(cb)
        })
      }
    }
  }


}
