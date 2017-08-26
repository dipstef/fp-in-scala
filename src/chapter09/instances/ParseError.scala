package chapter09.instances

import chapter09.ParsingError

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next else ""

  def columnCaret: String = (" " * (col - 1)) + "^"
}

case class ParseError(stack: List[(Location, String)] = List()) extends ParsingError {
  // The copy method comes for free with any case class. It returns a copy of the object, but with one or more
  // attributes modified.
  // If no new value is specified for a field, it will have the same value as in the original object. Behind the scenes,
  // this just uses the ordinary mechanism for default arguments in Scala.
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)

  // The intended meaning of label is that if p fails, its ParseError will somehow incorporate msg

  // a design decision that label trims the error stack, cutting off more detailed messages from inner scopes,
  // using only the most recent location from the bottom of the stack”
  def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)

  /** In the event of an error, returns the error that occurred most recently. */
  def latest: Option[(Location, String)] = stack.lastOption

  def latestLoc: Option[Location] = latest map (_._1)

  /**
    * Display collapsed error stack - any adjacent stack elements with the
    * same location are combined on one line. For the bottommost error, we
    * display the full line, with a caret pointing to the column of the error.
    * Example:
    * *
    *1.1 file 'companies.json'; array
    *5.1 object
    *5.2 key-value
    *5.10 ':'
    * *
    * { "MSFT" ; 24,
    */
  override def toString: String =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")

      collapsed.map { case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") + context
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = l.line + "." + l.col

}
