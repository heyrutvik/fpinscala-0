package catslib.trampoline

import scala.annotation.tailrec

sealed trait EvenOdd
case class Result(result: Boolean) extends EvenOdd
case class Even(value: Int) extends EvenOdd
case class Odd(value: Int) extends EvenOdd

object EvenOdd {

  def even(n: Int): EvenOdd = n match {
    case 0 => Result(true)
    case _ => Odd(n - 1)
  }

  def odd(n: Int): EvenOdd = n match {
    case 0 => Result(false)
    case _ => Even(n - 1)
  }

  @tailrec
  def run(evenOdd: EvenOdd): Boolean = evenOdd match {
    case Result(result) => result
    case Even(value) => run(even(value))
    case Odd(value) => run(odd(value))
  }

  def even1(n: Int): Boolean = n match {
    case 0 => true
    case _ => odd1(n - 1)
  }

  def odd1(n: Int): Boolean = n match {
    case 0 => false
    case _ => even1(n - 1)
  }

  //println(even(5000000))

  @tailrec
  def even2(n: Int): Boolean = n match {
    case 0 => true
    case 1 => false
    case _ => even2(n - 2)
  }

  println(even1(5000000))

  println(run(even(5000000)))
}
