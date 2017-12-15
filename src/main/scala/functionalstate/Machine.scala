package functionalstate

import Action._

case class Machine(locked: Boolean, candies: Int, coins: Int)

sealed trait Input
case object Coin extends Input
case object Turn extends Input

object Machine {

  type M[+A] = Action[Machine, Int]

  def update(i: Input)(m: Machine): Machine  = {
    (i, m) match {
      case (_, Machine(_, 0, _)) => m // no candies
      case (Coin, Machine(false, _, _)) => m // machine already unlocked
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1) // add coin
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
      case (Turn, Machine(true, _, _)) => m // turning on locked machine do nothing
    }
  }

  def simulateMachine(inputs: List[Input]): M[Int] = for {
    _ <- sequence(inputs.map(i => modify(update(i))))
    s <- get
  } yield (s.coins)

  def simulateMachine1(inputs: List[Input]): M[Int] = {
    sequence(inputs.map(i => modify(update(i)))).flatMap { v =>
      get.map(s => s.coins)
    }
  }

  def test = simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))

  def test1 = simulateMachine1(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
}