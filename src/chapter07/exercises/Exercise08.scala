package chapter07.exercises

import java.util.concurrent.Callable

import chapter07.Par.Par

/**
  * Hard: Take a look through the various static methods in Executors to get a feel for the different implementations of
  * ExecutorService that exist.
  *
  * Then, before continuing, go back and revisit your implementation of fork and try to find a counterexample or convince
  * yourself that the law holds for your implementation
  */
object Exercise08 {

  // Blocks for thread pool of fixed size 1,
  // The outer Callable gets submitted and picked up by the sole thread. Within that thread, before it will complete,
  // we submit and block waiting for the result of another Callable.
  // But there are no threads available to run this Callable. They’re waiting on each other and therefore our code deadlocks.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })


}
