package catslib.casestudy.mapreduce

import cats._
import cats.implicits._
import catslib.typeclasses.PrintableInstances._
import catslib.typeclasses.PrintableSyntax._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object MapReduce extends App {

  def foldMap[A, B](vs: Vector[A])(f: A => B)(implicit mb: Monoid[B]): B = vs.map(f).foldLeft(mb.empty)(mb.combine)

  foldMap(Vector(1,2,3))(identity).print
  foldMap(Vector(1,2,3))(_.toString + "!").print
  foldMap("Hello world!".toVector)(_.toString.toUpperCase).print

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    val groups = values.grouped(groupSize)
    val futures = groups map (g => Future(g.foldLeft(Monoid[B].empty)(_ |+| f(_))))
    Future.sequence(futures) map { iter =>
      iter.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

  def parallelFoldMapCats[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(g => Future(g.toVector.foldMap(f)))
      .map(_.combineAll)
  }

  val result: Future[Int] = parallelFoldMap((1 to 1000000).toVector)(identity)
  Await.result(result, 1.second).print
}
