package handlingerrors

import datastructure._
import scala.annotation._

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case l @ Left(_) => l
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case l @ Left(_) => l
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r @ Right(_) => r
    case _ => b
  }

  def map2[EE >: E,B,C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(x => b.map(y => f(x, y)))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)
  }

  def traverse[E,A,B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = l match {
    case Nil => Right(Nil)
    case Cons(h, t) => (f(h) map2 traverse(t)(f))(Cons(_,_))
  }

  def sequence[E,A](l: List[Either[E, A]]): Either[E, List[A]] = traverse(l)(x => x)
}
