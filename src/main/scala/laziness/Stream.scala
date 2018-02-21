package laziness

sealed trait Stream[+A] {

  import Stream._

  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case Some((a, as)) => a :: as.toList
    case None => Nil
  }

  def take(n: Long): Stream[A] = {
    def go(s: Stream[A], n: Long): Stream[A] = s.uncons match {
      case Some((a, as)) if n > 0 => cons(a, go(as, n - 1))
      case _ => empty
    }
    go(this, n)
  }

  def drop(n: Long): Stream[A] = {
    def go(s: Stream[A], n: Long): Stream[A] = s.uncons match {
      case Some((a, as)) if n > 0 => go(as, n - 1)
      case Some((_, as)) => s
      case None => empty
    }
    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def go(s: Stream[A]): Stream[A] = s.uncons match {
      case Some((a, as)) if p(a) => cons(a, go(as))
      case _ => empty
    }
    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    uncons match {
      case Some((h, t)) => {
//        println(s" => $h")
        f(h, t.foldRight(z)(f))
      }
      case None => z
    }
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // {println(s"$a"); p(a) || b}

  def forall(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile1(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A]) {
      case (a, z) if p(a) => cons(a, z)
      case (_, z) => z
    }

  def map[B](f: A => B): Stream[B] = uncons match {
    case Some((h, t)) => cons(f(h), t.map(f))
    case _ => empty
  }

  def map1[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, z) => cons(f(a), z))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A]) {
      case (a, z) if f(a) => cons(a, z)
      case (_, z) => z
    }

  def append[U >: A](other: Stream[U]): Stream[U] =
    foldRight(other)((a, z) => cons(a, z))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((a,z) => f(a).append(z))

  def map_2[B](f: A => B): Stream[B] = unfold(this) { s =>
    s.uncons match {
      case Some((a, as)) => Some((f(a), as))
      case _ => None
    }
  }

  def map2[B, C](s: Stream[B])(f: (A,B) => C): Stream[C] = {
    for (a <- this; b <- s) yield (f(a, b))
  }

  def takeWhile2(p: A => Boolean): Stream[A] = unfold(this) { s =>
    s.uncons match {
      case Some((a, as)) if p(a) => Some((a, as))
      case _ => None
    }
  }

  def take1(n: Long): Stream[A] = unfold((this, n)) {
    case (s, n) if n > 0 =>
      s.uncons match {
        case Some((a, as)) => Some((a, (as, n-1)))
        case _ => None
      }
    case _ => None
  }

  def zip[B](that: Stream[B]): Stream[(A, B)] = {
    unfold((this, that)) { case (a, b) =>
      (a.uncons, b.uncons) match {
        case (Some((a, as)), Some((b, bs))) => Some(((a, b), (as, bs)))
        case _ => None
      }
    }
  }

  def zipAll[C >: A, B](that: Stream[B], thisElem: C, thatElem: B): Stream[(C, B)] = {
    unfold((this, that)) { case (a, b) =>
      (a.uncons, b.uncons) match {
        case (Some((a, as)), Some((b, bs))) => Some(((a, b), (as, bs)))
        case (Some((a, as)), _) => Some(((a, thatElem), (as, empty)))
        case (_, Some((b, bs))) => Some(((thisElem, b), (empty, bs)))
        case _ => None
      }
    }
  }

  def tails: Stream[Stream[A]] = unfold(this) { s =>
    s.uncons match {
      case Some((_, as)) => Some((s, as))
      case _ => None
    }
  }

  def scanRight[B](z: => B)(f: (A,B) => B): Stream[B] = unfold(this) { s =>
    s.uncons match {
      case Some((_, as)) => Some((s.foldRight(z)(f(_,_)), as))
      case None => None
    }
  }
}

object Stream {

  def empty[A]: Stream[A] = {
    new Stream[A] {
      def uncons = None
    }
  }

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    new Stream[A] {
      lazy val uncons = Some((h, t))
    }
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def s1 = Stream(1,2,3,4,5,6,7,8,9,10).exists(_ == 5) // uncomment foldRight Cons println

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fib(n: Long): Stream[Long] = {
    def go(a: Long, b: Long): Stream[Long] = cons(a, go(b, a + b))
    go(0, 1).take(n)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }
  }

  def fib: Stream[Long] = unfold((0, 1)) {
    case (a, b) => Some((a, (b, a+b)))
  }

  def from1(n: Int): Stream[Int] = unfold(n)(n => Some(n, n+1))

  def constant(n: Int): Stream[Int] = unfold(n)(_ => Some(n, n))

  def ones = constant(1)

  def startWith[A](s1: Stream[A], s2: Stream[A]): Boolean = {
    s1.zip(s2).map_2{case (x, y) => x == y}.forall(_ == true)
  }

  def sum(a: Stream[Int]): Int = a.foldRight(0)(_ + _)

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails.exists(startWith(_, s2))
}