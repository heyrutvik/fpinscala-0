package catslib.casestudy.cedt

import cats._

trait BoundedSemiLattice[A] extends Monoid[A] {
  def combine(x: A, y: A): A
  def empty: A
}

object BoundedSemiLattice {

  implicit val intBoundedSemiLattice = new BoundedSemiLattice[Int] {
    override def empty: Int = 0
    override def combine(x: Int, y: Int): Int = x max y
  }

  implicit def setBoundedSemiLattice[A](implicit bs: BoundedSemiLattice[A]) = new BoundedSemiLattice[Set[A]] {
    override def empty: Set[A] = Set.empty
    override def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
  }
}