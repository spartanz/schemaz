package scalaz

package schema

object ScalaSchema {
  type Prim[A] = Nothing
}

object JsonSchema {
  type Prim[A] = JsonPrim[A]

  sealed trait JsonPrim[A]
  final case class JsonString(value: String) extends JsonPrim[String]
  final case class JsonNumber(value: BigDecimal) extends JsonPrim[BigDecimal]
  final case class JsonBool(value: Boolean) extends JsonPrim[Boolean]
  case object JsonNull extends JsonPrim[Nothing]
}
