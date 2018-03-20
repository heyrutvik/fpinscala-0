package catslib.trampoline

import scala.annotation.tailrec
import catslib.typeclasses.PrintableInstances._
import catslib.typeclasses.PrintableSyntax._

trait Computation[A]
class Continue[A](n: => Computation[A]) extends Computation[A] { lazy val next = n }
case class Done[A](result: A) extends Computation[A]

object Continue {
  def apply[A](n: => Computation[A]): Computation[A] = new Continue(n)
}

object Computation extends App {

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

  def factorial(n: BigInt): BigInt = {
    val one = BigInt(1)
    def go(n: BigInt, acc: BigInt): Computation[BigInt] = n match {
      case `one` => Done(acc)
      case n => Continue(go(n-1, n * acc))
    }
    val c: Computation[BigInt] = go(n, 1)
    run(c)
  }

  factorial(100000).print
}