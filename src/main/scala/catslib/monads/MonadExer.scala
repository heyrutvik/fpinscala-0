package catslib.monads

import cats._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MonadExer[F[_]] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  def map[A, B](value: F[A])(f: A => B): F[B] = flatMap(value)(a => pure(f(a)))
}

object MonadExer extends App {

  val opt1 = Monad[Option].pure(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))

  val fm = Monad[Future]
  val f1 = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(y => x*x + y*y))

  def sumSquareFor[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      x <- a
      y <- b
    } yield (x*x + y*y)
  }

  println(sumSquare(List(1,2,3), List(1,2,3)))
  println(sumSquare(Option(1), Option(2)))
}