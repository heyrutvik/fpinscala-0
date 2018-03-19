package catslib.casestudy.mapreduce

import cats._
import cats.implicits._
import catslib.typeclasses.PrintableInstances._
import catslib.typeclasses.PrintableSyntax._

object MapReduce extends App {

  def foldMap[A, B](vs: Vector[A])(f: A => B)(implicit mb: Monoid[B]): B = vs.map(f).foldLeft(mb.empty)(mb.combine)

  foldMap(Vector(1,2,3))(identity).print
  foldMap(Vector(1,2,3))(_.toString + "!").print
  foldMap("Hello world!".toVector)(_.toString.toUpperCase).print
}
