package scalaz

package schema

import Liskov._

object Json {
  type JSON = String

  type Encoder[A] = A => JSON

}

trait JsonModule[R <: Realisation] extends SchemaModule[R] {
  import Json._
  import SchemaF._

  implicit final def algebra(
    implicit primNT: R.Prim ~> Encoder,
    fieldLabel: R.ProductTermId <~< String,
    branchLabel: R.SumTermId <~< String
  ): HAlgebra[RSchema, Encoder] =
    new (RSchema[Encoder, ?] ~> Encoder) {

      val encloseInBraces         = (s: String) => s"{$s}"
      def makeField(name: String) = (s: String) => s""""$name":$s"""

      def apply[A](schema: RSchema[Encoder, A]): Encoder[A] =
        schema match {

          case PrimSchema(prim) => primNT(prim)
          case :*:(left, right) => (a => left(a._1) + "," + right(a._2))
          case :+:(left, right) => (a => a.fold(left, right))
          case i: RIso[Encoder, _, A] =>
            i.base.compose(i.iso.reverseGet)
          case r: RRecord[Encoder, _, A] =>
            encloseInBraces.compose(r.fields).compose(r.iso.reverseGet)
          case SeqSchema(element)    => (a => a.map(element).mkString("[", ",", "]"))
          case ProductTerm(id, base) => makeField(fieldLabel(id)).compose(base)
          case u: RUnion[Encoder, _, A] =>
            encloseInBraces.compose(u.choices).compose(u.iso.reverseGet)
          case SumTerm(id, base) => makeField(branchLabel(id)).compose(base)
          case One()             => (_ => "null")
        }
    }
}
