package scalaz

package schema

import Liskov._

object Json {
  type JSON = String

  type Encoder[A] = A => JSON

}

trait JsonModule[R <: Realisation] extends SchemaModule[R] {
  import Json._
  import Schema._

  implicit final def algebra(
    implicit primNT: R.Prim ~> Encoder,
    fieldLabel: R.ProductTermId <~< String,
    branchLabel: R.SumTermId <~< String
  ): HAlgebra[Schema[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], Encoder] =
    new (Schema[R.Prim, R.SumTermId, R.ProductTermId, Encoder, ?] ~> Encoder) {

      val encloseInBraces         = (s: String) => s"{$s}"
      def makeField(name: String) = (s: String) => s""""$name":$s"""

      def apply[A](schema: Schema[R.Prim, R.SumTermId, R.ProductTermId, Encoder, A]): Encoder[A] =
        schema match {

          case PrimSchema(prim) => primNT(prim)
          case :*:(left, right) => (a => left(a._1) + "," + right(a._2))
          case :+:(left, right) => (a => a.fold(left, right))
          case i: IsoSchema[R.Prim, R.SumTermId, R.ProductTermId, Encoder, _, A] =>
            i.base.compose(i.iso.reverseGet)
          case r: RecordSchema[R.Prim, R.SumTermId, R.ProductTermId, Encoder, A, _] =>
            encloseInBraces.compose(r.fields).compose(r.iso.reverseGet)
          case SeqSchema(element)    => (a => a.map(element).mkString("[", ",", "]"))
          case ProductTerm(id, base) => makeField(fieldLabel(id)).compose(base)
          case u: Union[R.Prim, R.SumTermId, R.ProductTermId, Encoder, A, _] =>
            encloseInBraces.compose(u.choices).compose(u.iso.reverseGet)
          case SumTerm(id, base) => makeField(branchLabel(id)).compose(base)
          case One()             => (_ => "null")
        }
    }
}
