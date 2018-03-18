package catslib.monads

import cats.data._

object StateExer extends App {

  val a = State[Int, String] { state =>
    (state + 10, s"The state is $state")
  }
  println(a.run(10).value)
  println(a.runS(10).value)
  println(a.runA(10).value)

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = {
    for {
      a <- step1
      b <- step2
    } yield (a, b)
  }

  val (state, result) = both.run(20).value
  println(s"state: $state and result: $result")

  import cats.data.State._
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
  val (s1, r1) = program.run(10).value
  println(s"state: $s1 and result: $r1")
}

object PostOrderCalc extends App {

  import cats.implicits._
  type CalcState[A] = State[List[Int], A]

  def parseInt(s: String) = scala.util.Try(s.toInt).toOption

  def operator(func: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
    case a :: b :: tail =>
      val ans = func(a, b)
      (ans :: tail, ans)
    case _ =>
      sys.error("Fail!")
  }

  def operand(num: Int): CalcState[Int] = State[List[Int], Int] { stack =>
    (num :: stack, 0)
  }

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+" => operator(_ + _)
      case "*" => operator(_ * _)
      case "-" => operator(_ - _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }
  }

  val program1 = for {
    _ <- evalOne("40")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield (ans)

  val program1Ans = program1.runA(Nil).value

  def evalAll(input: List[String]): CalcState[Int] = {
    val z = 0.pure[CalcState]
    input.foldLeft(z) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }
  }

  //evalAll for List("1", "2", "+", "3", "*")
  val evalAll1 = for {
    _ <- 0.pure[CalcState]
    _ <- evalOne("1")
    _ <- evalOne("2")
    _ <- evalOne("+")
    _ <- evalOne("3")
    ans <- evalOne("*")
  } yield (ans)

  //evalAll for List("1", "2", "+", "3", "*")
  val evalAll2 =
    0.pure[CalcState]
      .flatMap(_ => evalOne("1"))
      .flatMap(_ => evalOne("2"))
      .flatMap(_ => evalOne("+"))
      .flatMap(_ => evalOne("3"))
      .flatMap(_ => evalOne("*"))


  val program2 = evalAll(List("1", "2", "+", "3", "*"))
  val program2Ans = program2.runA(Nil).value

  val evalAll1Ans = evalAll1.runA(Nil).value
  val evalAll2Ans = evalAll2.runA(Nil).value

  if ((program2Ans == evalAll1Ans) && (program2Ans == evalAll2Ans) && (evalAll1Ans == evalAll2Ans)) println("yes") else println("no")

  def evalInput(exp: String): Int = {
    evalAll(exp.split(" ").toList).runA(Nil).value
  }

  def apply(exp: String) = evalInput(exp)
}