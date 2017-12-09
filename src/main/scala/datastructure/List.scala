package datastructure

import annotation.tailrec

sealed trait List[+A] {

  def take(n: Int): List[A] = {
    def go(l: List[A], n: Int): List[A] = l match {
      case Cons(_, _) if n == 0 => Nil
      case Cons(x, xs) => Cons(x, go(xs, n-1))
      case Nil => Nil
    }
    go(this, n)
  }

  def head: A = this match {
    case Cons(x, _) => x
    case _ => throw new Throwable("head of empty list")
  }

  def takeWhile(f: A => Boolean): List[A] = {
    def go(l: List[A]): List[A] = l match {
      case Cons(x, xs) if f(x) => Cons(x, go(xs))
      case Cons(x, _) if !f(x) => Nil
      case Nil => Nil
    }
    go(this)
  }

  def forall(f: A => Boolean): Boolean = {
    List.foldLeft(this, true)((z, x) => z && f(x))
  }

  def exists(f: A => Boolean): Boolean = {
    List.foldLeft(this, false)((z, x) => z || f(x))
  }

  def map1[B](f: A => B): List[B] = List.map1(this)(f)
}
case object Nil extends List[Nothing] {
  override def toString: String = "||"
}
case class Cons[+A](h: A, t: List[A]) extends List[A] {
  override def toString: String = h.toString + ", " + t
}

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + 1 // sum(t)
    case _ => 101
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  @tailrec def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n == 1 => xs
    case Cons(_, xs) => drop(xs, n-1)
    case Nil => Nil
  }

  @tailrec def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case l @ Cons(_, _) => l
    case Nil => Nil
  }

  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Cons(_, xs) => Cons(x, xs)
    case Nil => Nil
  }

  def reverse[A](l: List[A]): List[A] = {
    @tailrec def go(l: List[A], acc: List[A]): List[A] = l match {
      case Cons(x, xs) => go(xs, Cons(x, acc))
      case Nil => acc
    }
    go(l, Nil)
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec def go(l: List[A], acc: List[A]): List[A] = l match {
      case Cons(x, Nil) => reverse(acc)
      case Cons(x, xs) => go(xs, Cons(x, acc))
      case Nil => Nil
    }
    go(l, Nil)
  }

  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(l: List[Int]): Int = l match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum1(l: List[Int]): Int = foldRight(l, 0)(_ + _)

  def product1(l: List[Int]): Int = foldRight(l, 1)(_ * _)

  def createList[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])(Cons(_, _))

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, z) => 1 + z)

  @tailrec def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum2(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product2(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length1[A](l: List[A]): Int = foldLeft(l, 0)((z, _) => z + 1)

  // foldRight doesn't work. because it is lazy, it won't generate reverse!
  def reverse1[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((z, x) => Cons(x, z))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  def append1[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse1(a1), a2)((z, x) => Cons(x, z))

  def flat[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil: List[A])((l, z) => append2(l, z))

  def flat1[A](ls: List[List[A]]): List[A] = foldLeft(reverse1(ls), Nil: List[A])((z, l) => append2(l, z))

  // in terms of foldLeft
  def foldRight1[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse1(l), z)((x, z) => f(z,x))

  // in terms of foldRight
  def foldLeft1[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, z)((z, x) => f(x, z))

  def flat2[A](ls: List[List[A]]): List[A] = foldRight1(ls, Nil: List[A])((l, z) => append2(l, z))

  def flat3[A](ls: List[List[A]]): List[A] = foldLeft1(ls, Nil: List[A])((z, l) => append2(l, z))

  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, add1(xs))
  }

  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def map1[A,B](l: List[A])(f: A => B): List[B] = {
    @tailrec def go(l: List[A], acc: List[B]): List[B] = l match {
      case Nil => reverse1(acc)
      case Cons(x, xs) => go(xs, Cons(f(x), acc))
    }
    go(l, Nil)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(_, xs) => filter(xs)(f)
  }

  val even = filter(List(1,2,3,4,5,6,7))(x => x % 2 == 0)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => append2(f(x), flatMap(xs)(f))
  }

  def filter1[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(x => if(f(x)) List(x) else Nil)
  }

  def addCorresponding(a1: List[Int], a2: List[Int]): List[Int] = {
    (a1, a2) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addCorresponding(xs, ys))
      case (_, _) => Nil
    }
  }

  def zip[A, B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] = {
    (a1, a2) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zip(xs, ys)(f))
      case (_, _) => Nil
    }
  }

  def addCorresponding1(a1: List[Int], a2: List[Int]): List[Int] = zip(a1, a2)(_ + _)

  def slidingWindow[A](l: List[A], n: Int): List[List[A]] = {
    def go(l: List[A], acc: List[List[A]]): List[List[A]] = l match {
      case l @Cons(_, xs) if length1(l) >= n => go(xs, Cons(l.take(n), acc))
      case _ => acc
    }
    go(l, List(Nil))
  }

  def equal[A](a1: List[A], a2: List[A]): Boolean = {
    def go(a1: List[A], a2: List[A]): List[Boolean] = (a1, a2) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(x == y, go(xs, ys))
      case _ => Nil
    }
    if (length1(a1) == length1(a2)) go(a1, a2).forall(_ == true) else false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    slidingWindow(l, length1(sub)).map1((x: List[A]) => equal(x, sub)).exists(_ == true)
  }
}