package c12_applicative.exercises

import c12_applicative.Examples.{StreamApplicative, streamApplicative}

/**
  * “Hard: What is the meaning of streamApplicative.sequence? Specializing the signature of sequence to Stream, we have this:
  */
object Exercise04 {

  trait This extends StreamApplicative {

    // This transposes the list! That is, we start with a list of rows, each of which is possibly infinite in length.
    // We get back a single row, where each element is the column of values at that position.
    // Try it yourself in the REPL.
    override def sequence[A](fas: List[Stream[A]]): Stream[List[A]] = super.sequence(fas)
  }

  def main(args: Array[String]): Unit = {

      val s1 = streamApplicative.unit(1)
      val s2 = streamApplicative.unit(2)
      val s3 = streamApplicative.unit(3)
      val s4 = streamApplicative.unit(4)

      val sl = streamApplicative.sequence(List(s1, s2, s3, s4))

    println(s"sl.take(2) = ${sl.take(2).toList}")
  }

}
