package ru.orangepigment.exercises.chapter9

trait JSON

object JSON {
  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON
}

object Exercises {

  // parse a single digit, like '4', followed by that many 'a' characters
  def contextSensitiveParser[Parser[+_]](P: Parsers[Parser]): Parser[String] = {
    import P._
    "\\d".r flatMap (d => "a".listOfN(d.toInt).slice)
  }

  // ToDo: extra combinators can be used/introduced
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    import JSON._

    val spaces = char(' ').many.slice
    // FixME: JSON string has some limitations
    val jString = "\"\\w*\"".r.slice map JString
    val jNull = string("null").map(_ => JNull)
    val jBool = (string("true") | string("false")).map(s => JBool(s == "true"))
    // FixME: naive int number
    val jNumber = "-?\\d+".r.slice map (s => JNumber(s.toDouble))

    def jValue: Parser[JSON] = (spaces ** (jString | jNumber | jBool | jArray | jObject | jNull) ** spaces).map(_._1._2)

    def jArray = (char('[') ** (
      spaces.map(_ => JArray(IndexedSeq.empty[JSON])) |
        ((jValue ** char(',')).map(_._1).many ** jValue).map { case (l, j) => JArray((l :+ j).toIndexedSeq) }) **
      char(']')).map(_._1._2)

    def jKeyValue = (spaces ** jString ** spaces ** char(':') ** jValue).map(r => r._1._1._1._2.toString -> r._2)

    def jObject = (char('{') ** (
      spaces.map(_ => JObject(Map.empty[String, JSON])) |
        ((jKeyValue ** char(',')).map(_._1).many ** jKeyValue).map { case (l, j) => JObject((l :+ j).toMap) }) **
      char('}')).map(_._1._2)

    jObject | jArray
  }

}
