import scala.util.matching.Regex
//algebraic pattern (starting from the API)

//question how to parse 'a'

//def char(c: Char): Parser[Char]

// def run[A](p: Parser[A])(input: String):Either[ParseError,A]

/*
trait Parsers[ParseError, Parser[+_]] {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]

  // law: run(char(c))(c.toString) == Right(c)

  // how to parse "abracadabra"
  def string(s: String): Parser[String]

  // how to recognize either "abra" or "cadabra"
  // def orString(s1: String, s2: String): Parser[String]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
} */

//ParseError is a type argument so Parsers work for any representation
// [+_] is a type argument that is itself type constructor

trait Parsers[Parser[+_]] { self =>

  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1        => offset + 1
      case lineStart => offset - lineStart
    }
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  case class ParseError(stack: List[(Location, String)])
  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def defaultSucceed[A](a: A): Parser[A] = string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

  // 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  // 9.1
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a, la) => List(a) ++ la)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def slice[A](p: Parser[A]): Parser[String]

  // 9.4 HARD
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  // 9.1
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    // product(p, p2).map(f.tupled)
    p.flatMap(a => p2.map(b => f(a, b)))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many[A](p: Parser[A]): Parser[List[A]] = self.many(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(a => succeed(f(a)))

  // to label errors
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  // to give scope of parsing
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  // delays commiting to a fail
  def attempt[A](p: Parser[A]): Parser[A]

  // 9.6.1

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

// for implementation check this
// https://github.com/fpinscala/fpinscala/blob/first-edition/answers/src/main/scala/fpinscala/parsing/instances/Reference.scala
