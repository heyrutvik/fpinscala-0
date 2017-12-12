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
}