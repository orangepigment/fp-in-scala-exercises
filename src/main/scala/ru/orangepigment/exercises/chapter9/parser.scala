package ru.orangepigment.exercises.chapter9

import ru.orangepigment.exercises.chapter8.{Gen, Prop}

import scala.util.matching.Regex


trait Parsers[ParseError, Parser[+_]] {
  self =>

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def slice: Parser[String] = self.slice(p)

    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2) map f.tupled

  def map2ViaFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap(a => p2.map(f(a, _)))

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(f andThen succeed)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) {
      succeed(List())
    } else {
      map2(p, listOfN(n - 1, p))(_ :: _)
    }

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

}

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
  def contextSensitiveParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[String] = {
    import P._
    "\\d".r flatMap (d => "a".listOfN(d.toInt).slice)
  }

  // ToDo: extra combinators can be used/introduced
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
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