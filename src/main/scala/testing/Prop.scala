package testing

import functionalstate.RNG
import testing.Prop.{Result, TestCases}
import laziness._

case class Prop(run: (TestCases, RNG) => Result)

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  trait Status
  case object Proven extends Status
  case object Unfalsified extends Status

  type Result = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (n,rng) => f(n,rng) }
}
