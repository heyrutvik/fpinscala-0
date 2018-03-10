package testing

import functionalstate.RNG
import testing.Prop.{Result, TestCases}
import laziness._

case class Prop(run: (TestCases, RNG) => Result) {

  def &&(that: Prop): Prop = Prop {
    (n, rng) => {
      run(n, rng) match {
        case Right((_, i)) => that.run(n, rng)
        case s => s
      }
    }
  }

  def ||(that: Prop): Prop = Prop {
    (n, rng) => {
      run(n, rng) match {
        case Left(_) => that.run(n ,rng)
        case s => s
      }
    }
  }
}

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  trait Status
  case object Proven extends Status
  case object Unfalsified extends Status

  type Result = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => {
      def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result = {
        if (i == j) Right((Unfalsified, i))
        else {
          s.uncons match {
            case Some((Some(h), t)) => {
              try {
                if (f(h)) go(i+1, j, s, onEnd)
                else Left(h.toString)
              } catch {
                case e: Exception => Left(buildMsg(h, e))
              }
            }
            case Some((None, _)) => Right((Unfalsified, i))
            case None => onEnd(i)
          }
        }
      }
      go(0, n/3, a.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(a)(rng).map(Some(_))
          go(n/3, n, rands, i => Right((Unfalsified, i)))
        case s => s
      }
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (n,rng) => f(n,rng) }
}
