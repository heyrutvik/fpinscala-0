package catslib.monads.transformers

import scala.concurrent.ExecutionContext.Implicits.global
import cats.data._
import cats.implicits._
import catslib.monads.transformers.OptionTExer.tacticalReport

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object OptionTExer extends App {

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield (a + b)

  val foa = futureEitherOr.value
  val r = foa.value

  // exercise
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(a) => EitherT.right(Future(a))
      case None => EitherT.left(Future(s"$autobot unreachable"))
    }
  }

  def canSpecialMove(a1: String, a2: String): Response[Boolean] = {
    for {
      p1 <- getPowerLevel(a1)
      p2 <- getPowerLevel(a2)
    } yield (p1+p2 > 15)
  }

  def tacticalReport(a1: String, a2: String): String = {
    Await.result(canSpecialMove(a1, a2).value, 1.seconds) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$a1 and $a2 are ready to roll out!"
      case Right(false) => s"$a1 and $a2 need recharge."
    }
  }

  Seq(tacticalReport("Jazz", "Bumblebee"),
    tacticalReport("Bumblebee", "Hot Rod"),
    tacticalReport("Jazz", "Ironhide")) foreach println
}
