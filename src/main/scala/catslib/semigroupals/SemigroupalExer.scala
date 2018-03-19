package catslib.semigroupals

import cats._
import cats.implicits._

object SemigroupalExer extends App {

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = x.flatMap(a => y.map(b => (a, b)))
}
