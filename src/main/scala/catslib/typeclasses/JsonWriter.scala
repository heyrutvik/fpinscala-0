package catslib.typeclasses

trait JsonWriter[A] {
  def write(value: A): Json
}

object JsonWriter {

  implicit val stringToJson: JsonWriter[String] = value => JsString(value)
  implicit val intToJson: JsonWriter[Int] = value => JsInt(value)
  implicit val doubleToJson: JsonWriter[Double] = value => JsDouble(value)
  implicit val booleanToJson: JsonWriter[Boolean] = value => JsBoolean(value)

  implicit def optionToJson[A](implicit aWriter: JsonWriter[A]): JsonWriter[Option[A]] = { value =>
    value match {
      case Some(a) => aWriter.write(a)
      case None => JsNil
    }
  }

  implicit def personToString: JsonWriter[Person] = value => {
    JsObject {
      Map(
        "name" -> JsString(value.name),
        "age" -> JsInt(value.age),
        "married" -> Json.toJson(value.married)
      )
    }
  }
}