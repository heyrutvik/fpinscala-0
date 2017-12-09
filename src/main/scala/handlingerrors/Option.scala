package handlingerrors

import java.util.regex.{Pattern, PatternSyntaxException}
import datastructure._
import annotation._

trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case s @ Some(_) => s
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case s @ Some(x) if f(x) => s
    case None => None
  }

  def isDefined: Boolean = this match {
    case Some(_) => true
    case None => false
  }

  def get: A = this match {
    case Some(x) => x
    case None => throw new Throwable("get method on None undefined")
  }
}

case object None extends Option[Nothing]
case class Some[+A](value: A) extends Option[A]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = ???

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def pattern(s: String): Option[Pattern] = {
    try {
      Some(Pattern.compile(s))
    } catch {
      case _: PatternSyntaxException => None
    }
  }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat).map(p => (s: String) => p.matcher(s).matches()) // p => p.matcher(_).matches()

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }
  }

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat1), mkMatcher(pat2)) { (f1, f2) => //_(s) && _(s)
      f1(s) && f2(s)
    }
  }

  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    @tailrec def go(l: List[Option[A]], acc: List[A]): List[A] = l match {
      case Cons(Some(x), xs) => go(xs, Cons(x, acc))
      case Cons(_, _) => Nil
      case Nil => List.reverse1(acc)
    }
    val g = go(l, Nil)
    if (List.length1(g) > 0) Some(g) else None
  }

  def parsePatterns(l: List[String]): Option[List[Pattern]] =
    sequence(l map1 (pattern _))

  def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec def go(l: List[A], acc: List[B]): List[B] = l match {
      case Cons(x, xs) if f(x).isDefined => go(xs, Cons(f(x).get, acc))
      case Cons(x, xs) => Nil
      case _ => List.reverse1(acc)
    }
    val g = go(l, Nil)
    if (List.length1(g) > 0) Some(g) else None
  }

  val s1: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

  val s2: List[Option[Int]] = Cons(Some(1), Cons(Some(2), Cons(Some(3), Nil)))

  val s3: List[Option[Int]] = Cons(Some(1), Cons(None, Cons(Some(3), Nil)))

  def sequence1[A](l: List[Option[A]]): Option[List[A]] = traverse(l)(x => x)

  def parsePatterns1(l: List[String]): Option[List[Pattern]] = traverse(l)(pattern _)
}