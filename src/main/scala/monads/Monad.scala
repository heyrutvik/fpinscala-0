package monads

import functionalstate.Action

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = lma.foldRight(unit(List[A]()))((ma, b) => map2(ma, b)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => M[B]): M[List[B]] = as.foldRight(unit(List[B]()))((a, mbs) => map2(f(a), mbs)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] = e match {
    case Left(a) => map(a)(Left(_))
    case Right(a) => map(a)(Right(_))
  }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

  def flatMap1[A, B](ma: M[A])(f: A => M[B]): M[B] = compose((_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)
}

object Monad {

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type lambda[x] = Action[S, x]})#lambda] {
    def unit[A](a: => A): Action[S, A] = Action(s => (a, s))

    def flatMap[A, B](st: Action[S, A])(f: A => Action[S, B]): Action[S, B] =
      st flatMap f
  }
}

case class Id[A](value: A) {
  //  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

