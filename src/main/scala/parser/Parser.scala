package parser

import testing.{Gen, Prop}

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  //  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map(f.tupled)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def labelLaw[A](p: Parser[A], inputs: Gen[String]): Prop = ???
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')
  }

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  def errorStack(e: ParseError): List[(Location, String)]
}