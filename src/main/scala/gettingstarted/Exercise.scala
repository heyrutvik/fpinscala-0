package gettingstarted

import annotation.tailrec

object Exercise {

  def fib(n: Int): Int = {
    @tailrec def go(n: Int, a: Int, b: Int): Int = {
      if (n == 1) b
      else go(n-1, b, a + b)
    }
    if (n <= 1) n else go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    val max = as.length - 2 // slider based logic, 0->[0,1] 1->[1,2] 2->[2,3] for Array(1,2,3,4)
    @tailrec def go(n: Int, flag: Boolean): Boolean = {
      if (!flag || n == max) flag
      else go(n+1, gt(as(n), as(n+1)))
    }
    if (max >= 0) go(0, gt(as(0), as(1))) else true
  }

  def partial1[A,B,C](a: A, f: (A, B) => C): B => C = f(a, _)

  // concrete use of partial1
  val inc = partial1(1, (x: Int, y: Int) => x + y) // x => x + 1

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {(a: A) => f(a, _)} // = f(a, _)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {(a: A, b: B) => f(a)(b)} // = f(_, _)

  def identity[A](x: A): A = x

  def compose[A,B,C](f: B => C, g: A => B): A => C = {(a: A) => f(g(a))} // = f(g(_))
}