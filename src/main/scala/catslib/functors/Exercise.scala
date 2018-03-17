package catslib.functors

import cats.Functor
import cats.implicits._
import Tree._

object Exercise extends App {
  val box = Box[Int](123)
  println(box.map(_ + 1))

  val square = (x: Int) => x * x
  val squareAndAddFive = square.map(_ + 5).map(_ + 5)
  println(squareAndAddFive(2))

  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  println(tree.map(_ + 5))

  val l = List(1,2,3)
  println(l.map(_ + 1).map(_ + 1))
  println(l.flatMap(List(_)))
}

case class Box[A](value: A)

object Box {

  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))
  }
}
