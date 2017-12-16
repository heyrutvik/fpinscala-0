package parallelism

trait Par[+A]

object Par {

  def unit[A](a: => A): Par[A] = ???

  def get[A](a: Par[A]): A = ???

  def map2[A,B,C](p1: Par[A], p2: Par[B])(f: (A, B) => C): C = ???

  def sum(as: IndexedSeq[Int]): Int = {
    if (as.size <= 1) as.headOption getOrElse 0
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(unit(sum(l)), unit(sum(r)))(_ + _)
    }
  }
}
