package catslib.monads

import cats.Id

object Id {

  def pure[A](value: A): Id[A] = value

  def map[A, B](value: Id[A])(f: A => B): Id[B] = f(value)

  def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = f(value)
}