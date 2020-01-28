package schemaz

package generic
import scalaz.{ Alt, Decidable, \/, ~> }
import recursion._

trait GenericSchemaModule[R <: Realisation] extends SchemaModule[R] {

  def discardingFieldLabel[H[_]]: Field[H, ?] ~> H = λ[Field[H, ?] ~> H](field => field.schema)

  def discardingBranchLabel[H[_]]: Branch[H, ?] ~> H = λ[Branch[H, ?] ~> H](branch => branch.schema)

  def covariantTargetFunctor[H[_]](
    primNT: realisation.Prim ~> H,
    seqNT: H ~> λ[X => H[List[X]]],
    prodLabelNT: Field[H, ?] ~> H,
    sumLabelNT: Branch[H, ?] ~> H,
    delay: λ[X => () => H[X]] ~> H
  )(implicit H: Alt[H]): HAlgebra[RSchema, H] =
    new (RSchema[H, ?] ~> H) {

      def apply[A](schema: RSchema[H, A]): H[A] =
        schema match {
          case PrimSchemaF(prim)         => primNT(prim)
          case x: Sum[H, a, b]           => H.either2(x.left, x.right)
          case x: Prod[H, a, b]          => H.tuple2(x.left, x.right)
          case x: Record[H, a]           => x.fields
          case x: Sequence[H, a]         => seqNT(x.element)
          case pt: Field[H, a]           => prodLabelNT(pt)
          case x: Union[H, a]            => x.choices
          case st: Branch[H, a]          => sumLabelNT(st)
          case _: ROne[H]                => H.pure(())
          case ref @ SelfReference(_, _) => delay(() => ref.unroll)
        }
    }

  def contravariantTargetFunctor[H[_]](
    primNT: realisation.Prim ~> H,
    seqNT: H ~> λ[X => H[List[X]]],
    prodLabelNT: Field[H, ?] ~> H,
    sumLabelNT: Branch[H, ?] ~> H,
    delay: λ[X => () => H[X]] ~> H
  )(implicit H: Decidable[H]): HAlgebra[RSchema, H] =
    new (RSchema[H, ?] ~> H) {

      def apply[A](schema: RSchema[H, A]): H[A] =
        schema match {
          case PrimSchemaF(prim)         => primNT(prim)
          case x: Prod[H, a, b]          => H.divide(x.left, x.right)(identity[(a, b)])
          case x: Sum[H, a, b]           => H.choose(x.left, x.right)(identity[a \/ b])
          case x: Record[H, a]           => x.fields
          case x: Sequence[H, a]         => seqNT(x.element)
          case pt: Field[H, a]           => prodLabelNT(pt)
          case x: Union[H, a]            => x.choices
          case st: Branch[H, a]          => sumLabelNT(st)
          case _: ROne[H]                => H.xproduct0[Unit](())
          case ref @ SelfReference(_, _) => delay(() => ref.unroll)
        }
    }
}
