package testing

import functionalstate.{Action, RNG}
import laziness._

case class Gen[+A](sample: Action[RNG, A], exhaustive: Stream[A])

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(Action.unit(a), Stream(a))

  def boolean: Gen[Boolean] = Gen(Action.unit(false), Stream(false, true))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(
      Action(RNG.positiveInt1).map(i => start + i % (stopExclusive - start)),
      Stream(start until stopExclusive: _*))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(
      Action.sequence(List.fill(n)(g.sample)),
      Stream.empty
    )
  }
}
