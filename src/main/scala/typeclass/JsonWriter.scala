package typeclass

trait JsonWriter[A] {
  def write(value: A): Json
}

object JsonWriter {

  implicit val stringToString: JsonWriter[String] = value => JsString(value)
  implicit val intToString: JsonWriter[Int] = value => JsInt(value)
  implicit val doubleToString: JsonWriter[Double] = value => JsDouble(value)
  implicit val optionToString: JsonWriter[Option[_]] = value => optionToString(value)
  implicit val personToString: JsonWriter[Person] = value => {
    JsObject {
      Map(
        "name" -> JsString(value.name),
        "age" -> JsInt(value.age),
        "married" -> optionToString(value.married)
      )
    }
  }

  private def optionToString[A](value: A) = {
    value match {
      case Some(x) => JsString(x.toString)
      case None => JsNil
    }
  }
}