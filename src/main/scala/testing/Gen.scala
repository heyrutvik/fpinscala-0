package testing

import functionalstate.{Action, RNG}
import laziness._
import testing.Gen.Domain

case class Gen[+A](sample: Action[RNG, A], exhaustive: Domain[A]) {

  import Gen._

  def map[B](f: A => B): Gen[B] = {
    Gen(
      sample.map(f),
      exhaustive.map(x => x.map(f).orElse(None))
    )
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(
      sample.map2(g.sample)(f),
      exhaustive.map2(g.exhaustive) {
        case (Some(a), Some(b)) => Option(f(a, b))
        case _ => None
      }
    )
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(
      sample.flatMap(f(_).sample),
      exhaustive.flatMap(x => x.map(f(_).exhaustive).getOrElse(unbounded))
    )
  }
}

object Gen {

  val rand = RNG.simple(10)

  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] = Gen(Action.unit(a), bounded(Stream(a)))

  def boolean: Gen[Boolean] = Gen(Action(RNG.boolean), bounded(Stream(false, true)))

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

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

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 != 0) n + 1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 != 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 == 0) n + 1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for {
    i <- choose(from, to)
    j <- if (i % 2 == 0) even(from, to) else odd(from, to)
  } yield (i, j)
}
