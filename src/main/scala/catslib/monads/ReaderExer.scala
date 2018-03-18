package catslib.monads

import catslib.typeclasses._
import cats.data._

object ReaderExer extends App {

  val cat1 = Cat("c1", 12, "black")
  val catName: Reader[Cat, String] = Reader(cat => cat.name)
  println(catName.run(cat1))
  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")
  println(greetKitty.run(cat1))
  val colorKitty: Reader[Cat, String] = Reader(cat => s"you have a really nice color ${cat.color}")
  val greetColorKitty: Reader[Cat, String] = {
    for {
      greet <- greetKitty
      color <- colorKitty
    } yield s"$greet. $color"
  }
  println(greetColorKitty(cat1))


  //exercise
  case class Db(
    username: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader( db =>
    db.username.get(userId)
  )

  def checkPassword(username: String, password: String): DbReader[Boolean] = Reader( db =>
    db.passwords.get(username).contains(password)
  )

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      userOpt <- findUsername(userId)
      value <- userOpt.map(u => checkPassword(u, password)).getOrElse(Reader(_ => false): DbReader[Boolean])
    } yield (value)
  }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))
}