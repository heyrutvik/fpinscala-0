package externaleffects

trait IO[+A] {
  self =>
  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] {
    override def run: B = f(self.run)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    override def run: B = f(self.run).run
  }
}

object IO {

  def unit[A](a: => A): IO[A] = new IO[A] {
    override def run: A = a
  }

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

  def apply[A](a: => A): IO[A] = unit(a)
}