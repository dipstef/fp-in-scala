package chapter09

import language.higherKinds
import language.implicitConversions

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _,   _}

    implicit val parser: Parsers[Parser] = P

    // Unfortunately the implicit conversion is not working here
    implicit def tok[Parser_[+_]](s: String)(implicit P:Parsers[Parser_]): Parser_[String] = P.token(P.string(s))

    def array: Parser[JSON] = surround("[", "]")(value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"

    def obj = surround("{", "}")(keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"

    def keyval = escapedQuoted ** (tok(":") *> value)

    def lit: Parser[JSON] = {
      def jNull = tok("null").as(JNull)
      def jNumber = double.map(JNumber)
      def jString = escapedQuoted.map(JString)
      def jBool = tok("true").as(JBool(true)) | tok("false").as(JBool(false))

      scope("literal") {jNull | jNumber | jString | jBool}
    }

    def value: Parser[JSON] = lit | obj | array| root(whitespace *> (obj | array))


    value
  }


}