package chapter07.exercises

import chapter07.Par.Par


trait Exercise01 {
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
}
