package functionalstate

import annotation._

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  def simple(seed: Long): RNG = new RNG {
    val seed2 =
      (seed*0x5DEECE66DL + 0xBL) &
      ((1L << 48) - 1)

    def nextInt = ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Int.MinValue, nextState) => positiveInt(nextState)
      case r @ (x, _) if x > 0 => r
      case (x, nextState) => (x.abs, nextState)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = positiveInt(rng)
    (i / (Int.MaxValue.toDouble), r) //
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, s1) = rng.nextInt
    val (d, s2) = double(s1)
    ((i, d), s2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, s1) = double(rng)
    val (i, s2) = s1.nextInt
    ((d, i), s2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(rng)
    val (d3, s3) = double(rng)
    ((d1, d2, d3), s3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec def go(state: RNG, acc: List[Int], n: Int): (List[Int], RNG) = {
      val (i, s) = state.nextInt
      if (n > 0) go(s, i :: acc, n - 1)
      else (acc, s)
    }
    go(rng, Nil, count)
  }

  /**
    * better API for state actions
    */

  type RandAction[+A] = RNG => (A, RNG)

  val int: RandAction[Int] = s => s.nextInt

  def unit[A](a: A): RandAction[A] = rng => (a, rng)

  def map[A, B](action: RandAction[A])(f: A => B): RandAction[B] = {
    rng => {
      val (v, s1) = action(rng)
      (f(v), s1)
    }
  }

  //def positiveMax1(n: Int)(rng: RNG): (Int, RNG) = map(_.nextInt)(_ % n)(rng)
  def positiveMax(n: Int): RandAction[Int] = map(int)(_ % n)

  def double1: RandAction[Double] = map(int)(_ / Int.MaxValue.toDouble)

  def map2[A,B,C](action1: RandAction[A], action2: RandAction[B])(f: (A,B) => C): RandAction[C] = {
    rng => {
      val (i, s1) = action1(rng)
      val (d, s2) = action2(s1)
      (f(i, d), s2)
    }
  }

  def intDouble: RandAction[(Int, Double)] = map2(positiveMax(Int.MaxValue), double1)((_, _))

  def doubleInt: RandAction[(Double, Int)] = map2(double1, positiveMax(Int.MaxValue))((_, _))

  def sequence[A](actions: List[RandAction[A]]): RandAction[List[A]] = {
    rng => {
      @tailrec def go(as: List[(RandAction[A])], acc: List[A], s: RNG): (List[A], RNG) = {
        as match {
          case h :: t =>
            val (v, rng1) = h(rng)
            go(t, v :: acc, rng1)
          case _ => (acc.reverse, s)
        }
      }
      go(actions, Nil, rng)
    }
  }

  def sequence1[A](actions: List[RandAction[A]]): RandAction[List[A]] = {
    actions.foldRight(unit(Nil: List[A]))((a, z) => map2(a, z)(_ :: _))
  }

  def ints1(count: Int): RandAction[List[Int]] = sequence(List.fill(count)(int))

  def positiveInt1: RandAction[Int] = {
    flatMap(int) { i =>
      if (i != Int.MaxValue) unit(i.abs) else positiveInt1
    }
  }

  def flatMap[A,B](action: RandAction[A])(f: A => RandAction[B]): RandAction[B] = {
    rng =>
      val (a, s1) = action(rng)
      f(a)(s1)
  }
}