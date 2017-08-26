package chapter09

import chapter09.instances.Reference


case class Parse[Parser[+ _]](P: Parsers[Parser]) {
  import P._

  val spaces: Parser[List[String]] = " ".many

  val magicSpell: Parser[((String, List[String]), String)] = scope("magic spell") {"abra" ** spaces ** "cadabra"}
  val gibberish: Parser[((String, List[String]), String)] = scope("gibberish") { "abba" ** spaces ** "babba"}


}

object ParsersExample extends App {

  import Reference._

  val p = Parse(Reference)

  def printResult[E](e: Either[E,_]): Unit = e.fold(println, println)

  printResult { Reference.run(p.magicSpell)("abra cadabra") }
  printResult { Reference.run(p.magicSpell)("abracadabra") }

  println("Failure on first word:")
  printResult { Reference.run(p.magicSpell)("abrAcadabra") }

  println("Failure on second word:")
  printResult { Reference.run(p.magicSpell)("abracAdabra") }

  println("Failure due not using attempt on first parser:")
  // will fail because the failure is committed on the first parser as we are not using attempt
  printResult { Reference.run(p.magicSpell or p.gibberish)("abba babba") }
  // will succeed
  printResult { run(attempt(p.magicSpell) or p.gibberish)("abba babba") }

}

