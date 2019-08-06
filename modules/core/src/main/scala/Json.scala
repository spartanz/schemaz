package schemaz

import scalaz.~>
import scalaz.Liskov.<~<

object Json {
  type JSON = String

  type Encoder[A] = A => JSON

}

trait JsonModule[R <: Realisation] extends SchemaModule[R] {
  import Json._

  implicit final def encoderInterpreter(
    implicit primNT: R.Prim ~> Encoder,
    fieldLabel: R.ProductTermId <~< String,
    branchLabel: R.SumTermId <~< String
  ): RInterpreter[Encoder] =
    Interpreter.cata[RSchema, Encoder](new (RSchema[Encoder, ?] ~> Encoder) {

      val encloseInBraces         = (s: String) => s"{$s}"
      def makeField(name: String) = (s: String) => s""""$name":$s"""

      def apply[A](schema: RSchema[Encoder, A]): Encoder[A] =
        schema match {

          case p: RPrim[Encoder, a] => primNT(p.prim)
          case ProdF(left, right)   => (a => left(a._1) + "," + right(a._2))
          case SumF(left, right)    => (a => a.fold(left, right))
          case i: IsoSchema[Encoder, _, a] =>
            i.base.compose(i.iso.reverseGet)
          case r: Record[Encoder, _, a] =>
            encloseInBraces.compose(r.fields).compose(r.iso.reverseGet)
          case SeqF(element)    => (a => a.map(element).mkString("[", ",", "]"))
          case FieldF(id, base) => makeField(fieldLabel(id)).compose(base)
          case u: Union[Encoder, _, a] =>
            encloseInBraces.compose(u.choices).compose(u.iso.reverseGet)
          case BranchF(id, base)         => makeField(branchLabel(id)).compose(base)
          case One()                     => (_ => "null")
          case ref @ SelfReference(_, _) => (a => ref.unroll(a))
        }
    })
}
