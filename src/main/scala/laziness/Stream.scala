package laziness

sealed trait Stream[+A] {

  import Stream._

  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case Some((a, as)) => a :: as.toList
    case None => Nil
  }

  def take(n: Int): Stream[A] = {
    def go(s: Stream[A], n: Int): Stream[A] = s.uncons match {
      case Some((a, as)) if n > 0 => cons(a, go(as, n - 1))
      case _ => empty
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
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

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
}