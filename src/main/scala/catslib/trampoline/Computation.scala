package catslib.trampoline

import scala.annotation.tailrec

trait Computation[A]
class Continue[A](n: => Computation[A]) extends Computation[A] {
  lazy val next = n
}
case class Done[A](result: A) extends Computation[A]

object Continue {
  def apply[A](n: => Computation[A]): Computation[A] = new Continue(n)
}

object Computation {

  def even(n: Int): Computation[Boolean] = n match {
    case 0 => Done(true)
    case _ => Continue(odd(n - 1))
  }

  def odd(n: Int): Computation[Boolean] = n match {
    case 0 => Done(false)
    case _ => Continue(even(n - 1))
  }

  @tailrec
  def run[A](computation: Computation[A]): A = computation match  {
    case Done(a) => a
    case c: Continue[A] => run(c.next)
  }
}