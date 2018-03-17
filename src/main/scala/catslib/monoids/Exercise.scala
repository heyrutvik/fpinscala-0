package catslib.monoids

import cats.Monoid
import cats.implicits._

object Exercise extends App {

  def add[A](items: List[A])(implicit m: Monoid[A]): A = items.foldLeft(m.empty)(_ |+| _)

  println(add(List(1, 2, 3)))
  println(add(List(Some(1), None, Some(2), None, Some(3))))
  println(add(List(Order(1, 2), Order(3, 4), Order(5, 6))))
}

case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    override def empty: Order = Order(0, 0)
  }
}