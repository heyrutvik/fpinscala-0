package monads

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  def traverse[A,B](as: List[A])(f: A => M[B]): M[List[B]] = as.foldRight(unit(List[B]()))((a, mbs) => map2(f(a), mbs)(_ :: _))
}
