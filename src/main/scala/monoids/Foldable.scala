package monoids

trait Foldable[F[_]] {

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(Monoid.endoMonoid[B])(z)
  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(m: Monoid[B]): B = foldRight(as)(m.zero)((a, b) => m.op(f(a), b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
}