package fpinscala.parsing

import fpinscala.testing.{Gen, Prop}

import scala.language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char]

  def a0: Parser[Int]

  def a1: Parser[Int]

  def ab: Parser[(Int, Int)]

  def **[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] = product(p, p2)

  def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => flatMap(p2)(b => succeed(a -> b)))

  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => map(p2)(b => f(a, b)))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(f.andThen(succeed))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)


  def toLazy[A](p: => Parser[A]): Parser[A] = {
    lazy val r = p
    r
  }

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, toLazy(many(p)))(_ :: _) or succeed(List())

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many1: Parser[List[A]] = self.map2(p, many)(_ :: _)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: Parser[B]): Parser[(A, B)] =
      self.product(p, p2)
  }

  object Laws {
    run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    run(listOfN(3, "cad" | "ab"))("ababcad") == Right("cadabab")
    run(listOfN(3, "cad" | ("ab" | "fg")))("fgababcad") == Right("cadababfg")
    run(a0)("ababcad") == Right(3)
    run(a1)("sdsdsdd") == Left(ParseError(List(Location("sdsdsdd") -> "at least one a is expected")))
    run(a1)("dafsdf") == Right(1)
    run(ab)("aaaab") == Right(4 -> 1)

    char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}