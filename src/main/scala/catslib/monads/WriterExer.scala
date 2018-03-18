package catslib.monads

import cats.data.Writer
import cats.implicits._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object WriterExer extends App {

  // Writer apply
  val w = Writer(Vector("It was the best of times", "It was the worst of times"), 1859)

  // alias for log.. Vector[String] Monad need in scope
  type Logged[A] = Writer[Vector[String], A]

  // create value of Logged
  val value = 123.pure[Logged]

  // create log where value is Unit
  val log = Vector("msg1", "msg2").tell

  // Writer syntax
  val both = 123.writer(Vector("msg1"))
  // extract both at the same time
  val (l, v) = both.run
  println(s"$v and $l")

  // flatMap and map operation preserves log
  val w1 = {
    for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield (a + b)
  }.mapWritten(_.map(_.toUpperCase))
  val (l1, v1) = w1.run
  println(s"$v1 and $l1")

  // exercise
  def slowly[A](body: => A) = try body finally Thread.sleep(700)
  def factorial(n: Int): Int = {
    def fact(nw: Logged[Int]): Logged[Int] = {
      if (nw.value == 0) {
        1.pure[Logged]
      } else {
        for {
          n <- nw
          ans <- slowly(fact((n-1).pure[Logged]).map(_ * n))
          _ <- Vector(s"${Thread.currentThread().getName} $n $ans").tell
        } yield (ans)
      }
    }
    val (l, v) = fact(n.pure[Logged]).run
    println(l)
    v
  }
  Await.result(Future.sequence(
    Vector(Future(factorial(3)), Future(factorial(5)), Future(factorial(8)), Future(factorial(2)))
  ), 10.seconds)
}
