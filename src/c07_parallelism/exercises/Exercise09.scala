package c07_parallelism.exercises


/**
  * Hard: Show that any fixed-size thread pool can be made to deadlock given this implementation of fork.
  */
object Exercise09 {

  /*
    For a thread pool of size 2, `fork(fork(fork(x)))` will deadlock, and so on. Another, perhaps more interesting
    example is `fork(map2(fork(x), fork(y)))`.

    In this case, the outer task is submitted first and occupies a thread waiting for both `fork(x)` and `fork(y)`.
    The `fork(x)` and `fork(y)` tasks are submitted and run in parallel, except that only one thread is available,
    resulting in deadlock.
  */
}
