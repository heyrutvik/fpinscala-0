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
    @tailrec def go(l: List[Int], state: RNG, n: Int): (List[Int], RNG) = {
      val (i, s) = state.nextInt
      if (n > 0) go(i :: l, s, n - 1)
      else (l, s)
    }
    go(Nil, rng, count)
  }

  /**
    * better API for state actions
    */

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = r => r.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (v, s1) = s(rng)
      (f(v), s1)
    }
  }

  //def positiveMax1(n: Int)(rng: RNG): (Int, RNG) = map(_.nextInt)(_ % n)(rng)
  def positiveMax(n: Int): Rand[Int] = map(_.nextInt)(_ % n)

  def double1: Rand[Double] = map(_.nextInt)(_ / Int.MaxValue.toDouble)(_)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (i, s1) = ra(rng)
      val (d, s2) = rb(s1)
      (f(i, d), s2)
    }
  }

  def intDouble: Rand[(Int, Double)] = map2(positiveMax(Int.MaxValue), double1)((_, _))

  def doubleInt: Rand[(Double, Int)] = map2(double1, positiveMax(Int.MaxValue))((_, _))

  def identity[A](x: A) = x

  def sequence[A](fs: List[(Rand[A])]): Rand[List[A]] = {
    rnd => {
      def go(l: List[(Rand[A])], acc: List[A], s: RNG): (List[A], RNG) = {
        l match {
          case r :: rs =>
            val (a, rnd1) = map(r)(identity)(s)
            go(rs, a :: acc, rnd1)
          case _ => (acc.reverse, s)
        }
      }
      go(fs, Nil, rnd)
    }
  }

  def ints1(count: Int): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))

  def positiveInt1: Rand[Int] = {
    map(int) { i =>
      if (i != Int.MaxValue) i.abs else ???
    }
  }

  def flatMap[A,B](s: Rand[A])(f: A => Rand[B]): Rand[B] = {
    rng =>
      val (a, s1) = map(s)(identity)(rng)
      f(a)(s1)
  }
}