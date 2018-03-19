package catslib.monads

import catslib.functors._
import cats._
import cats.implicits._

object Custom extends App {

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      case Leaf(x) => f(x)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      f(a) match {
        case Branch(l, r) =>
          Branch(
            flatMap(l) {
              case Left(l) => tailRecM(l)(f)
              case Right(l) => pure(l)
            },
            flatMap(r) {
              case Left(r) => tailRecM(r)(f)
              case Right(r) => pure(r)
            }
          )
        case Leaf(Left(v)) => tailRecM(v)(f)
        case Leaf(Right(v)) => Leaf(v)
      }
    }
  }

  println(branch(Leaf(10), Leaf(11)).flatMap(x => Leaf(x + 1)))
}
