package c10_monoids

import c10_monoids.Monoid.foldMapV

// The partial result of the word count could be represented by an algebraic data type:
sealed trait WC

// A Stub is the simplest case, where we haven’t seen any complete words yet.
case class Stub(chars: String) extends WC

// a Part keeps the number of complete words we’ve seen so far, in words. The value lStub holds any partial word we’ve
// seen to the left of those words, and rStub holds any partial word on the right
case class Part(lStub: String, words: Int, rStub: String) extends WC


object WC {

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    // The empty result, where we haven't seen any characters yet.
    val zero = Stub("")

    def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  def count(s: String): Int = {
    // A single character's count. Whitespace does not count and non-whitespace starts a new Stub.
    def wc(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(`s`) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
}
