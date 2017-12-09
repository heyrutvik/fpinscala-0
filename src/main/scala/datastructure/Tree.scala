package datastructure

sealed trait Tree[+A] {

  def size: Int = {
    def go(t: Tree[A]): Int = t match {
      case Branch(l, r) => 1 + go(l) + go(r)
      case Leaf(_) => 1
    }
    go(this)
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Branch(l, r) => Branch(l.map(f), r.map(f))
    case Leaf(x) => Leaf(f(x))
  }

  def maximum[U >: A](z: U)(gt: (U, U) => U): U = {
    def go(t: Tree[U], max: U): U = t match {
      case Branch(l, r) => gt(go(l, max), go(r, max))
      case Leaf(x) => x
    }
    go(this, z)
  }

  def depth: Int = {
    def go(t: Tree[A], d: Int): Int = t match {
      case Branch(l, r) => go(l, d+1) max go(r, d+1)
      case Leaf(_) => d
    }
    go(this, 0)
  }

  def fold[U >: A](z: U)(f: (U, U) => U): U = {
    def go(t: Tree[U], z: U): U = t match {
      case Branch(l, r) => f(go(l, z), go(r, z))
      case Leaf(x) => f(x, z)
    }
    go(this, z)
  }
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  val t1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(12)), Branch(Leaf(3), Leaf(4)))

  val t2: Tree[String] = Branch(Branch(Leaf("a"), Leaf("bbbb")), Branch(Leaf("ccc"), Leaf("dd")))

  val t3: Tree[Int] = {
    Branch(
      Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)),
      Branch(Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))), Leaf(1))
    )
  }
}