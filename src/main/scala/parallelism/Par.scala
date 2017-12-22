package parallelism

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def map2_1[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = { s =>
    val f1 = p1(s)
    val f2 = p2(s)
    unit(f(f1.get, f2.get))(s)
  }

  def fork[A](a: => Par[A]): Par[A] = { s =>
    s.submit(() => a(s).get())
  }

  def async[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def sum(as: IndexedSeq[Int]): Par[Int] = {
    if (as.size <= 1) unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2_1(sum(l), sum(r))(_ + _)
    }
  }

  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    map2_1(l, unit(()))((a, _) => a.sorted)

  def map1[A, B](fa: Par[A])(f: A => B): Par[B] = map2_1(fa, unit(()))((a, _) => f(a))

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = s => UnitFuture((run(s)(fa).get, run(s)(fb).get))

  def map[A, B](fa: Par[A])(f: A => B): Par[B] = s => UnitFuture(f(run(s)(fa).get))

  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = {
    map(product(p1, p2)) {
      case (a, b) => f(a, b)
    }
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] = {
    l.foldRight(unit[List[A]](Nil))((a, z) => map2(a, z)(_ :: _))
  }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    fork {
      sequence(l.map(asyncF(f)))
    }
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    fork {
      sequence(l.filter(f).map(unit))
    }
  }

  def equals[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(e).get == p2(e).get

  def test1 = {
    val a = async(42 + 1)
    val s = Executors.newFixedThreadPool(1)
    println(Par.equals(s)(a, fork(a)))
  }

  def delay[A](a: => Par[A]): Par[A] = { s =>
    a(s)
  }

  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = { s =>
    run(s)(a).get match {
      case a if a > 0 && a < choices.length => choices(a)(s)
    }
  }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    choiceN(map(a)(if (_) 1 else 0))(List(ifFalse, ifTrue))
  }

  def choiceMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] = { s =>
    run(s)(choices(run(s)(a).get()))
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = { s =>
    run(s)(f(run(s)(a).get))
  }

  def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)
}
