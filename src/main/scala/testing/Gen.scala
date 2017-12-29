package testing

import functionalstate.{Action, RNG}

object Gen {

  type Gen[A] = Action[RNG, A]

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Action(RNG.int).map(i => i % stopExclusive)
}
