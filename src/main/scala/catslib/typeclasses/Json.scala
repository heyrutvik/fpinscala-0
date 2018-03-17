package catslib.typeclasses

sealed trait Json
case class JsObject(value: Map[String, Json]) extends Json
case class JsString(value: String) extends Json
case class JsInt(value: Int) extends Json
case class JsDouble(value: Double) extends Json
case class JsBoolean(value: Boolean) extends Json
case object JsNil extends Json

object Json {
  def toJson[A](value: A)(implicit converter: JsonWriter[A]): Json = converter.write(value)
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }
}