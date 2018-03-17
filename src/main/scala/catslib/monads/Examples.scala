package catslib.monads

object Examples extends App {

  def parseInt(str: String): Option[Int] = scala.util.Try(str.toInt).toOption

  def parseIntF: String => Option[Int] = parseInt _

  def divide(a: Int, b: Int): Option[Int] = if (b == 0) None else Some(a / b)

  def divideF: Int => Option[Int] = divide(10, _)

  //using flatMap
  def stringDivideByFlatMap(aStr: String, bStr: String): Option[Int] = {
    parseInt(aStr).flatMap(a => parseInt(bStr).flatMap(b => divide(a, b)))
  }

  //using map
  def stringDivideByMap(aStr: String, bStr: String): Option[Option[Option[Int]]] = {
    parseInt(aStr).map(a => parseInt(bStr).map(b => divide(a, b)))
  }

  //using for comprehension
  def stringDivideByFor(aStr: String, bStr: String): Option[Int] = {
    for {
      a <- parseInt(aStr)
      b <- parseInt(bStr)
      z <- divide(a, b)
    } yield (z)
  }

  def associativity(m: Option[String]) = {
    m.flatMap(parseIntF).flatMap(divideF) == m.flatMap(s => parseIntF(s).flatMap(divideF))
  }

  println(associativity(Option("10")))
}
