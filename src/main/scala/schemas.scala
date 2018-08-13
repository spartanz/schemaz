package scalaz

package schema

object ScalaSchema {
  type Prim[A] = Nothing
}

object JsonSchema {
  type Prim[A] = JsonPrim

  sealed trait JsonPrim
  case object JsonString extends JsonPrim
  case object JsonNumber extends JsonPrim
  case object JsonBool extends JsonPrim
  case object JsonNull extends JsonPrim
}
