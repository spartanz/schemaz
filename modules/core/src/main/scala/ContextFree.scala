package scalaz

package schema

trait ContextFreeAlgebras extends SchemaModule {

  import Schema._

  type Id[A] = A

  def covariantTargetFunctor[H[_]](
    primNT: Prim ~> H,
    seqNT: H ~> λ[X => H[List[X]]],
    prodLabelNT: λ[X => H[(ProductTermId, X)]] ~> H,
    sumLabelNT: λ[X => H[(SumTermId, X)]] ~> H,
    one: H[Unit]
  )(implicit H: Alt[H]): HAlgebra[Schema, H] = new (Schema[H, ?] ~> H) {

    def apply[A](schema: Schema[H, A]): H[A] = schema match {
      case PrimSchema(prim)          => primNT(prim)
      case :*:(left, right)          => H.tuple2(left, right)
      case :+:(left, right)          => H.either2(left, right)
      case IsoSchema(base, iso)      => H.map(base)(iso.get)
      case RecordSchema(fields, iso) => H.map(fields)(iso.get)
      case SeqSchema(element)        => seqNT(element)
      case ProductTerm(id, base)     => prodLabelNT(H.tuple2(H.pure(id), base))
      case Union(choices, iso)       => H.map(choices)(iso.get)
      case SumTerm(id, base)         => sumLabelNT(H.tuple2(H.pure(id), base))
      case One()                     => one
    }
  }



  def contravariantTargetFunctor[H[_]](
    primNT: Prim ~> H,
    seqNT: H ~> λ[X => H[List[X]]],
    prodLabelNT: λ[X => H[(ProductTermId, X)]] ~> H,
    sumLabelNT: λ[X => H[(SumTermId, X)]] ~> H,
    pure: Id ~> H,
    one: H[Unit]
  )(implicit H: Decidable[H]): HAlgebra[Schema, H] = new (Schema[H, ?] ~> H) {

    def apply[A](schema: Schema[H, A]): H[A] = schema match {
      case PrimSchema(prim) => primNT(prim)
      case :*:(left, right) => H.tuple2(left, right)
      case x: :+:[_, a, b]  => H.choose(x.left, x.right)(identity[a \/ b])
      //UHOH THOSE BOTH COMPILE?! (for the love of all that is precious to you, please leave the pattern matches that actually bind the type variables)
      //case IsoSchema(base, iso)      => H.contramap(base)(iso.get)
      //case IsoSchema(base, iso)      => H.contramap(base)(iso.reverseGet)
      //Luckily does not compile
      //case x: IsoSchema[_, a, a0]    => H.contramap(x.base)(x.iso.get)
      case x: IsoSchema[_, a, a0]    => H.contramap(x.base)(x.iso.reverseGet)
      case x: RecordSchema[_, a, a0] => H.contramap(x.fields)(x.iso.reverseGet)
      case SeqSchema(element)        => seqNT(element)
      case ProductTerm(id, base)     => prodLabelNT(H.tuple2(pure(id), base))
      case x: Union[_, a, a0]       => H.contramap(x.choices)(x.iso.reverseGet)
      case SumTerm(id, base)         => sumLabelNT(H.tuple2(pure(id), base))
      case One()                     => one
    }
  }

}
