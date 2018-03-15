package monoids

case class Shape[A](isInShape: A => Boolean)

object Shape {

  type Coord2d = (Double, Double)
  type Shape2d = Shape[Coord2d]

  def outside[A](shape: Shape[A]): Shape[A] = Shape { s =>
    !shape.isInShape(s)
  }

  def distance(a: Coord2d, b: Coord2d) = Math.sqrt(Math.pow((a._2 - a._1), 2) + Math.pow((b._2 - b._1), 2))

  def disk(center: Coord2d, r: Double): Shape2d = Shape { c =>
    distance(center, c) <= r
  }

  def intersect[A](s1: Shape[A], s2: Shape[A]): Shape[A] = Shape { s =>
    s1.isInShape(s) && s2.isInShape(s)
  }

  def allShape[A]: Shape[A] = Shape[A](_ => true)
}