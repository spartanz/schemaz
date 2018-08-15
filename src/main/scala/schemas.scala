package scalaz

package schema

object ScalaSchema {
  type Prim[A] = Nothing
}

object JsonSchema {
  type Prim[A] = JsonPrim[A]

  sealed trait JsonPrim[A]
  final case object JsonString extends JsonPrim[String]
  final case object JsonNumber extends JsonPrim[BigDecimal]
  final case object JsonBool   extends JsonPrim[Boolean]
  final case object JsonNull   extends JsonPrim[Null]
}
