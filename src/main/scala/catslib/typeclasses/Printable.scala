package catslib.typeclasses

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
  def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}

object PrintableInstances {

  implicit val stringFormat: Printable[String] = identity(_)
  implicit val intFormat: Printable[Int] = value => value.toString
  implicit val catFormat: Printable[Cat] = { value =>
    val name = Printable.format(value.name)
    val age = Printable.format(value.age)
    val color = Printable.format(value.color)
    s"$name is a $age year-old $color cat"
  }
}