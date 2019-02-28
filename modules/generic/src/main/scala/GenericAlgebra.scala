package scalaz

package schema

package generic

import recursion._

trait GenericSchemaModule[R <: Realisation] extends SchemaModule[R] {

  def covariantTargetFunctor[H[_]](
    primNT: R.Prim ~> H,
    seqNT: H ~> λ[X => H[List[X]]],
    prodLabelNT: RProductTerm[H, ?] ~> H,
    sumLabelNT: RSumTerm[H, ?] ~> H,
    one: H[Unit]
  )(implicit H: Alt[H]): HAlgebra[RSchema, H] =
    new (RSchema[H, ?] ~> H) {

      def apply[A](schema: RSchema[H, A]): H[A] =
        schema match {
          case PrimSchema(prim)       => primNT(prim)
          case x: RSum[H, a, b]       => H.either2(x.left, x.right)
          case x: RProduct[H, a, b]   => H.tuple2(x.left, x.right)
          case x: RIso[H, a0, a]      => H.map(x.base)(x.iso.get)
          case x: RRecord[H, a0, a]   => H.map(x.fields)(x.iso.get)
          case x: RSeq[H, a]          => seqNT(x.element)
          case pt: RProductTerm[H, a] => prodLabelNT(pt)
          case x: RUnion[H, a0, a]    => H.map(x.choices)(x.iso.get)
          case st: RSumTerm[H, a]     => sumLabelNT(st)
          case _: ROne[H]             => one
          case SelfReference(unroll, nt)    => ???
        }
    }

  def contravariantTargetFunctor[H[_]](
    primNT: R.Prim ~> H,
    seqNT: H ~> λ[X => H[List[X]]],
    prodLabelNT: RProductTerm[H, ?] ~> H,
    sumLabelNT: RSumTerm[H, ?] ~> H,
    one: H[Unit]
  )(implicit H: Decidable[H]): HAlgebra[RSchema, H] =
    new (RSchema[H, ?] ~> H) {

      def apply[A](schema: RSchema[H, A]): H[A] =
        schema match {
          case PrimSchema(prim)     => primNT(prim)
          case x: RProduct[H, a, b] => H.divide(x.left, x.right)(identity[(a, b)])
          case x: RSum[H, a, b]     => H.choose(x.left, x.right)(identity[a \/ b])
          //UHOH THOSE BOTH COMPILE?! (for the love of all that is precious to you, please leave the pattern matches that actually bind the type variables)
          //case IsoSchema(base, iso)      => H.contramap(base)(iso.get)
          //case IsoSchema(base, iso)      => H.contramap(base)(iso.reverseGet)
          //Luckily does not compile
          //case x: IsoSchema[_, a, a0]    => H.contramap(x.base)(x.iso.get)
          case x: RIso[H, a, a0]      => H.contramap(x.base)(x.iso.reverseGet)
          case x: RRecord[H, a, a0]   => H.contramap(x.fields)(x.iso.reverseGet)
          case x: RSeq[H, a]          => seqNT(x.element)
          case pt: RProductTerm[H, a] => prodLabelNT(pt)
          case x: RUnion[H, a0, a]    => H.contramap(x.choices)(x.iso.reverseGet)
          case st: RSumTerm[H, a]     => sumLabelNT(st)
          case _: ROne[H]             => one
          case SelfReference(unroll, nt)    => ???
        }
    }

}
