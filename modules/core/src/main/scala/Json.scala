package scalaz

package schema

import Liskov._

trait JsonModule extends SchemaModule {
  type JSON = String

  type Encoder[A] = A => JSON

  import Schema._

  implicit final def algebra(
    implicit primNT: Prim ~> Encoder,
    fieldLabel: ProductTermId <~< String,
    branchLabel: SumTermId <~< String
  ): HAlgebra[Schema, Encoder] = new (Schema[Encoder, ?] ~> Encoder) {

    val encloseInBraces         = (s: String) => s"{$s}"
    def makeField(name: String) = (s: String) => s""""$name":$s"""

    def apply[A](schema: Schema[Encoder, A]): Encoder[A] = schema match {

      case PrimSchema(prim)          => primNT(prim)
      case :*:(left, right)          => (a => left(a._1) + "," + right(a._2))
      case :+:(left, right)          => (a => a.fold(left, right))
      case IsoSchema(base, iso)      => base.compose(iso.reverseGet)
      case RecordSchema(fields, iso) => encloseInBraces.compose(fields).compose(iso.reverseGet)
      case SeqSchema(element)        => (a => a.map(element).mkString("[", ",", "]"))
      case ProductTerm(id, base)     => makeField(fieldLabel(id)).compose(base)
      case Union(choices, iso)       => encloseInBraces.compose(choices).compose(iso.reverseGet)
      case SumTerm(id, base)         => makeField(branchLabel(id)).compose(base)
      case One()                     => (_ => "null")
    }
  }
}
