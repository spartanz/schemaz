package scalaz

package schema

package generic

trait GenericAlgebra extends SchemaModule {

  import Schema._

  type Id[A] = A

  def covariantTargetFunctor[H[_]](
    primNT: Prim ~> H,
    seqNT: H ~> λ[X => H[List[X]]],
    prodLabelNT: λ[X => H[(ProductTermId, X)]] ~> H,
    sumLabelNT: λ[X => H[(SumTermId, X)]] ~> H,
    one: H[Unit]
  )(implicit H: Alt[H]): HAlgebra[Schema[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], H] =
    new (Schema[R.Prim, R.SumTermId, R.ProductTermId, H, ?] ~> H) {

      def apply[A](schema: Schema[R.Prim, R.SumTermId, R.ProductTermId, H, A] @unchecked): H[A] =
        schema match {
          case PrimSchema(prim)                                      => primNT(prim)
          case x: :+:[R.Prim, R.SumTermId, R.ProductTermId, H, a, b] => H.either2(x.left, x.right)
          case x: :*:[H, a, b, R.Prim, R.SumTermId, R.ProductTermId] =>
            H.tuple2(x.left, x.right)
          case x: IsoSchema[R.Prim, R.SumTermId, R.ProductTermId, H, a, a0] =>
            H.map(x.base)(x.iso.get)
          case x: Record[R.Prim, R.SumTermId, R.ProductTermId, H, a, a0] =>
            H.map(x.fields)(x.iso.get)
          case x: SeqSchema[H, a, R.Prim, R.SumTermId, R.ProductTermId] => seqNT(x.element)
          case ProductTerm(id, base)                                    => prodLabelNT(H.tuple2(H.pure(id), base))
          case x: Union[R.Prim, R.SumTermId, R.ProductTermId, H, a, a0] =>
            H.map(x.choices)(x.iso.get)
          case SumTerm(id, base)                               => sumLabelNT(H.tuple2(H.pure(id), base))
          case _: One[R.Prim, R.SumTermId, R.ProductTermId, H] => one
        }
    }
  }

  def contravariantTargetFunctor[H[_]](
    primNT: Prim ~> H,
    seqNT: H ~> λ[X => H[List[X]]],
    prodLabelNT: λ[X => H[(ProductTermId, X)]] ~> H,
    sumLabelNT: λ[X => H[(SumTermId, X)]] ~> H,
    pure: Id ~> H,
    one: H[Unit]
  )(implicit H: Decidable[H]): HAlgebra[Schema[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], H] =
    new (Schema[R.Prim, R.SumTermId, R.ProductTermId, H, ?] ~> H) {

      def apply[A](schema: Schema[R.Prim, R.SumTermId, R.ProductTermId, H, A] @unchecked): H[A] =
        schema match {
          case PrimSchema(prim) => primNT(prim)
          case x: :*:[H, a, b, R.Prim, R.SumTermId, R.ProductTermId] =>
            H.divide(x.left, x.right)(identity[(a, b)])
          case x: :+:[R.Prim, R.SumTermId, R.ProductTermId, H, a, b] =>
            H.choose(x.left, x.right)(identity[a \/ b])
          //UHOH THOSE BOTH COMPILE?! (for the love of all that is precious to you, please leave the pattern matches that actually bind the type variables)
          //case IsoSchema(base, iso)      => H.contramap(base)(iso.get)
          //case IsoSchema(base, iso)      => H.contramap(base)(iso.reverseGet)
          //Luckily does not compile
          //case x: IsoSchema[_, a, a0]    => H.contramap(x.base)(x.iso.get)
          case x: IsoSchema[R.Prim, R.SumTermId, R.ProductTermId, H, a, a0] =>
            H.contramap(x.base)(x.iso.reverseGet)
          case x: Record[R.Prim, R.SumTermId, R.ProductTermId, H, a, a0] =>
            H.contramap(x.fields)(x.iso.reverseGet)
          case x: SeqSchema[H, a, R.Prim, R.SumTermId, R.ProductTermId] => seqNT(x.element)
          case ProductTerm(id, base)                                    => prodLabelNT(H.tuple2(pure(id), base))
          case x: Union[R.Prim, R.SumTermId, R.ProductTermId, H, a, a0] =>
            H.contramap(x.choices)(x.iso.reverseGet)
          case SumTerm(id, base)                               => sumLabelNT(H.tuple2(pure(id), base))
          case _: One[R.Prim, R.SumTermId, R.ProductTermId, H] => one
        }
    }
  }

}
