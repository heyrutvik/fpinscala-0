package testing

import functionalstate.{Action, RNG}
import laziness._
import testing.Gen.Domain

case class Gen[+A](sample: Action[RNG, A], exhaustive: Domain[A])

object Gen {

  val rand = RNG.simple(10)

  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] = Gen(Action.unit(a), bounded(Stream(a)))

  def boolean: Gen[Boolean] = Gen(Action(RNG.boolean), bounded(Stream(false, true)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(
      Action(RNG.positiveInt1).map(i => start + i % (stopExclusive - start)),
      bounded(Stream.from(start).take(stopExclusive - start))
    )
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(Action.sequence(List.fill(n)(g.sample)),
      unbounded) // TODO
  }

  def uniform: Gen[Double] = Gen(Action(RNG.double1), unbounded)

  def choose(start: Double, stopExclusive: Double): Gen[Double] =
    Gen(Action(RNG.double1).map(i => start + i * (stopExclusive - i)), unbounded)
}
