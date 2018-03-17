package catslib.typeclasses

import JsonSyntax._

object JsonWriterTest extends App {
  val p1 = Person("p1", 26, Some(true))
  val p2 = Person("p2", 19, None)
  List(p1, p2).foreach { p =>
    println(Json.toJson(p)) // using object
    println(p.toJson) // using
  }
}

case class Person(name: String, age: Int, married: Option[Boolean])