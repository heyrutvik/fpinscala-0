package catslib.functors

trait Codec[A] {
  self =>
  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))
      override def decode(value: String): B = dec(self.decode(value))
    }
  }
}

object Codec {

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)
}

object CodecInstances {

  implicit val stringCodec: Codec[String] = {
    new Codec[String] {
      override def encode(value: String): String = value

      override def decode(value: String): String = value
    }
  }

  implicit val intCodec: Codec[Int] = stringCodec.imap[Int](_.toInt, _.toString)
  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap[Boolean](_.toBoolean, _.toString)
  implicit val doubleCodec: Codec[Double] = stringCodec.imap[Double](_.toDouble, _.toString)
  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap[Box[A]](Box(_), _.value)
}
