package handlingerrors

import java.util.regex.{Pattern, PatternSyntaxException}
import datastructure.List

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

  def sequence[A](l: List[Option[A]]): Option[List[A]] = ???
}