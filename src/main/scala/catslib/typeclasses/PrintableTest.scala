package catslib.typeclasses

import PrintableInstances._
import PrintableSyntax._
import cats.implicits._
import cats.kernel.Eq
import catslib.functors.Box

object PrintableTest extends App {

  val c1 = Cat("c1", 4, "black")
  val c2 = Cat("c2", 6, "pink")

  val optionCat1 = Option(c1)
  val optionCat2 = Option.empty[Cat]

  Printable.print(c1)
  c1.print

  c1 === c2
  optionCat1 =!= optionCat2

  Box(true).print
}

case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catEq: Eq[Cat] = { (cat1, cat2) =>
    cat1.name === cat2.name && cat1.age === cat2.age && cat1.color === cat2.color
  }
}