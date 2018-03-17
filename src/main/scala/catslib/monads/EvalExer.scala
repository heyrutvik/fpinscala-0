package catslib.monads

import cats._

object EvalExer extends App {

  val greeting =
    Eval.always {
      println("step1")
      "Hello"
    }.map { str =>
      println("step2")
      s"$str world"
    }

  println(greeting.value)


  val ans = for {
    a <- Eval.now {
      println("Cal A"); 40
    }
    b <- Eval.always {
      println("Cal B"); 2
    }
  } yield (a + b)

  println(ans.value)
  println(ans.value)


  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) Eval.now(n) else Eval.defer(factorial(n - 1).map(_ * n))

  println(factorial(50000).value)


  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = {
      as match {
        case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
        case Nil => acc
      }
    }

    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value
  }

  println(foldRight((1 to 100000).toList, 0)(_ + _))
}
