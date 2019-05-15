package scalaz

package schema
import scala.annotation.implicitNotFound

import recursion._

import monocle.Iso
import shapeless._

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

object Representation {
  type RSum[RA, A, RB, B]
  type RProd[RA, A, RB, B]
  type RIso[RA, A, B]
  type RSelf[A]
  type RSeq[R, A]
  type -*>[K, V]
  type -+>[K, V]
  type RRecord[RA, An, A]
  type RUnion[RA, An, A]
}

import Representation._

sealed trait SchemaF[Prim[_], SumTermId, ProductTermId, F[_, _], R, A] {
  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, R, A]
}

trait SelfRef              {}
trait FieldR[Repr]         {}
trait BranchR[Repr]        {}
trait UnionR[Repr, AE, A]  {}
trait RecordR[Repr, AP, A] {}
trait IsoR[Repr, A, B]     {}

////////////////////
// The Schema ADT
////////////////////

// "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

final case class One[F[_, _], Prim[_], SumTermId, ProductTermId]()
    extends SchemaF[Prim, SumTermId, ProductTermId, F, Unit, Unit] {
  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, Unit, Unit] = One()
}

/**
 * The sum of two schemas, yielding the schema for `A \/ B`
 */
final case class SumF[F[_, _], RA, RB, A, B, Prim[_], SumTermId, ProductTermId](
  left: F[RA, A],
  right: F[RB, B]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RSum[RA, A, RB, B], A \/ B] {

  def hmap[G[_, _]](
    nt: F ~~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, RSum[RA, A, RB, B], A \/ B] =
    SumF(
      nt(left),
      nt(right)
    )
  override def toString: String = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class ProdF[F[_, _], RA, RB, A, B, Prim[_], SumTermId, ProductTermId](
  left: F[RA, A],
  right: F[RB, B]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RProd[RA, A, RB, B], (A, B)] {

  def hmap[G[_, _]](
    nt: F ~~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, RProd[RA, A, RB, B], (A, B)] =
    ProdF(nt(left), nt(right))
  override def toString: String = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchemaF[F[_, _], A, Prim[_], SumTermId, ProductTermId](prim: Prim[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, Prim[A], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, Prim[A], A] =
    PrimSchemaF[G, A, Prim, SumTermId, ProductTermId](prim)
}

/**
 * A named branch of an union
 */
final case class BranchF[F[_, _], RA, I <: SumTermId, A, Prim[_], SumTermId, ProductTermId](
  id: I,
  schema: F[RA, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, -+>[I, RA], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, -+>[I, RA], A] =
    BranchF(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 * This class cannot be constructed directly, you must use the `SchemaModule#union` method.
 */
final case class UnionF[F[_, _], RA: IsUnion, A, AE, Prim[_], SumTermId, ProductTermId](
  choices: F[RA, AE],
  iso: Iso[AE, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RUnion[RA, AE, A], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RUnion[RA, AE, A], A] =
    UnionF[G, RA, A, AE, Prim, SumTermId, ProductTermId](nt(choices), iso)
}

/**
 * A named field of a record
 */
final case class FieldF[F[_, _], RA, I <: ProductTermId, A, Prim[_], SumTermId, ProductTermId](
  id: I,
  schema: F[RA, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, -*>[I, RA], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, -*>[I, RA], A] =
    FieldF(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 * This class cannot be constructed directly, you must use the `SchemaModule#record` method.
 */
final case class RecordF[F[_, _], RA: IsRecord, A, AP, Prim[_], SumTermId, ProductTermId](
  fields: F[RA, AP],
  iso: Iso[AP, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RRecord[RA, AP, A], A] {

  def hmap[G[_, _]](
    nt: F ~~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, RRecord[RA, AP, A], A] =
    RecordF[G, RA, A, AP, Prim, SumTermId, ProductTermId](nt(fields), iso)
}

/**
 * A sequence
 */
final case class SeqF[F[_, _], RA, A, Prim[_], SumTermId, ProductTermId](element: F[RA, A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, RSeq[RA, A], List[A]] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RSeq[RA, A], List[A]] =
    SeqF(nt(element))
}

/**
 * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
 * between AO and A, then a schema of A0 can be used to represent values of A.
 */
final case class IsoSchemaF[F[_, _], RA, A0, A, Prim[_], SumTermId, ProductTermId](
  base: F[RA, A0],
  iso: Iso[A0, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RIso[RA, A0, A], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RIso[RA, A0, A], A] =
    IsoSchemaF(nt(base), iso)
}

final case class SelfReference[F[_, _], H[_, _], A, Prim[_], SumTermId, ProductTermId](
  private val ref: () => F[_, A],
  private val nattrans: F ~~> H
) extends SchemaF[Prim, SumTermId, ProductTermId, H, RSelf[A], A] {

  lazy val unroll: H[_, A] = nattrans(ref())

  def hmap[G[_, _]](nt: H ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RSelf[A], A] =
    SelfReference[F, G, A, Prim, SumTermId, ProductTermId](ref, nt.compose(nattrans))
}

/**
 * An interpreter able to derive a `F[A]` from a schema for `A` (for any `A`).
 * Such interpreters will usually be implemented using a recursion scheme like
 * 'cataNT`or hyloNT`.
 */
trait Interpreter[F[_, _], G[_, _]] { self =>

  /**
   * A natural transformation that will transform a schema for any type `A`
   * into an `F[A]`.
   */
  def interpret: F ~~> G

  def compose[H[_, _]](nt: H ~~> F) = self match {
    case i: ComposedInterpreter[h, G, F] => ComposedInterpreter(i.underlying, i.nt.compose(nt))
    case x                               => ComposedInterpreter(x, nt)
  }
}

final case class ComposedInterpreter[F[_, _], G[_, _], H[_, _]](
  underlying: Interpreter[F, G],
  nt: H ~~> F
) extends Interpreter[H, G] {
  final override val interpret = underlying.interpret.compose(nt)
}

class CataInterpreter[S[_[_, _], _, _], F[_, _]](
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[Fix[S, ?, ?], F] {
  final override val interpret = cataNT(algebra)
}

class HyloInterpreter[S[_[_, _], _, _], F[_, _], G[_, _]](
  coalgebra: HCoalgebra[S, G],
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[G, F] {
  final override val interpret = hyloNT(coalgebra, algebra)
}

@implicitNotFound(
  msg = "It seems like the following representation type isn't isomorphic to a product of named fields: ${A}"
)
trait IsRecord[A]

object IsRecord {
  implicit def singleFieldIsRecord[K, V]: IsRecord[K -*> V] = new IsRecord[K -*> V] {}

  implicit def productIsRecord[L: IsRecord, R: IsRecord, X, Y]: IsRecord[RProd[L, X, R, Y]] =
    new IsRecord[RProd[L, X, R, Y]] {}

  implicit def isoIsRecord[R: IsRecord, A0, A]: IsRecord[RIso[R, A0, A]] =
    new IsRecord[RIso[R, A0, A]] {}
}

@implicitNotFound(
  msg = "It seems like the following representation type isn't isomorphic to a sum of named branches: ${A}"
)
trait IsUnion[A]

object IsUnion {

  implicit def singleBranchIsUnion[K, V]: IsUnion[K -+> V] = new IsUnion[K -+> V] {}

  implicit def productIsUnion[L: IsUnion, R: IsUnion, X, Y]: IsUnion[RSum[L, X, R, Y]] =
    new IsUnion[RSum[L, X, R, Y]] {}
}

object SchemaF {

  implicit def schemaHFunctor[Prim[_], SumTermId, ProductTermId] =
    new HFunctor[SchemaF[Prim, SumTermId, ProductTermId, ?[_, _], ?, ?]] {

      def hmap[F[_, _], G[_, _]](nt: F ~~> G) =
        new (SchemaF[Prim, SumTermId, ProductTermId, F, ?, ?] ~~> SchemaF[
          Prim,
          SumTermId,
          ProductTermId,
          G,
          ?,
          ?
        ]) {
          def apply[R, A](fa: SchemaF[Prim, SumTermId, ProductTermId, F, R, A]) = fa.hmap(nt)
        }
    }

}

trait SchemaModule[R <: Realisation] {

  val R: R

  type RInterpreter[F[_, _]] = Interpreter[Schema, F]

  type RSchema[F[_, _], Repr, A] = SchemaF[R.Prim, R.SumTermId, R.ProductTermId, F, Repr, A]

  type BareSchema[A] = Fix[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_, _], ?, ?], _, A]

  type Schema[Repr, A] = Fix[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_, _], ?, ?], Repr, A]

  type Schema_[A] = BareSchema[A]

  type ROne[F[_, _]]               = One[F, R.Prim, R.SumTermId, R.ProductTermId]
  type RPrim[F[_, _], A]           = PrimSchemaF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Sum[F[_, _], RA, RB, A, B]  = SumF[F, RA, RB, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Prod[F[_, _], RA, RB, A, B] = ProdF[F, RA, RB, A, B, R.Prim, R.SumTermId, R.ProductTermId]

  type Branch[F[_, _], RA, I <: R.SumTermId, A] =
    BranchF[F, RA, I, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Union[F[_, _], RA, AE, A] = UnionF[F, RA, A, AE, R.Prim, R.SumTermId, R.ProductTermId]

  type Field[F[_, _], RA, I <: R.ProductTermId, A] =
    FieldF[F, RA, I, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Record[F[_, _], RA, An, A]   = RecordF[F, RA, A, An, R.Prim, R.SumTermId, R.ProductTermId]
  type Sequence[F[_, _], RA, A]     = SeqF[F, RA, A, R.Prim, R.SumTermId, R.ProductTermId]
  type IsoSchema[F[_, _], RA, A, B] = IsoSchemaF[F, RA, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Self[F[_, _], A]             = SelfReference[Any, F, A, R.Prim, R.SumTermId, R.ProductTermId]

  object Interpreter {

    def cata[S[_[_, _], _, _], F[_, _]](alg: HAlgebra[S, F])(implicit ev: HFunctor[S]) =
      new CataInterpreter[S, F](alg)

    def hylo[S[_[_, _], _, _], F[_, _], G[_, _]](coalg: HCoalgebra[S, G], alg: HAlgebra[S, F])(
      implicit ev: HFunctor[S]
    ) = new HyloInterpreter(coalg, alg)

  }

  implicit class FixProdSyntax[RA, RB, A, B](schema: Schema[RProd[RA, A, RB, B], (A, B)]) {

    def left: Schema[RA, A] =
      schema.unFix.asInstanceOf[Prod[Schema, RA, RB, A, B]].left

    def right: Schema[RB, B] =
      schema.unFix.asInstanceOf[Prod[Schema, RA, RB, A, B]].right

  }

  implicit class FixSumSyntax[RA, RB, A, B](schema: Schema[RSum[RA, A, RB, B], A \/ B]) {

    def left: Schema[RA, A] =
      schema.unFix.asInstanceOf[Sum[Schema, RA, RB, A, B]].left

    def right: Schema[RB, B] =
      schema.unFix.asInstanceOf[Sum[Schema, RA, RB, A, B]].right

  }

  implicit class FixFieldSyntax[I <: R.ProductTermId, RA, A](schema: Schema[I -*> RA, A]) {
    def id: I               = schema.unFix.asInstanceOf[Field[Schema, RA, I, A]].id
    def base: Schema[RA, A] = schema.unFix.asInstanceOf[Field[Schema, RA, I, A]].schema
  }

  implicit class FixBranchSyntax[I <: R.SumTermId, RA, A](schema: Schema[I -+> RA, A]) {
    def id: I               = schema.unFix.asInstanceOf[Branch[Schema, RA, I, A]].id
    def base: Schema[RA, A] = schema.unFix.asInstanceOf[Branch[Schema, RA, I, A]].schema
  }

  implicit class FixRecordSyntax[RA, An, A](schema: Schema[RRecord[RA, An, A], A]) {
    def fields: Schema[RA, An] = schema.unFix.asInstanceOf[Record[Schema, RA, An, A]].fields
    def iso: Iso[An, A]        = schema.unFix.asInstanceOf[Record[Schema, RA, An, A]].iso
  }

  implicit class FixUnionSyntax[RA, Ae, A](schema: Schema[RUnion[RA, Ae, A], A]) {
    def choices: Schema[RA, Ae] = schema.unFix.asInstanceOf[Union[Schema, RA, Ae, A]].choices
    def iso: Iso[Ae, A]         = schema.unFix.asInstanceOf[Union[Schema, RA, Ae, A]].iso
  }

  implicit class FixIsoSyntax[RA, A0, A](schema: Schema[RIso[RA, A0, A], A]) {
    def base: Schema[RA, A0] = schema.unFix.asInstanceOf[IsoSchema[Schema, RA, A0, A]].base
    def iso: Iso[A0, A]      = schema.unFix.asInstanceOf[IsoSchema[Schema, RA, A0, A]].iso
  }

  implicit class FixSeqSyntax[RA, A](schema: Schema[RSeq[RA, A], List[A]]) {
    def element: Schema[RA, A] = schema.unFix.asInstanceOf[Sequence[Schema, RA, A]].element
  }

  implicit class FixPrimSyntax[A](schema: Schema[R.Prim[A], A]) {
    def prim: R.Prim[A] = schema.unFix.asInstanceOf[RPrim[Schema, A]].prim
  }

  sealed abstract class Derivation[G[_, _], Repr, A, ReprOut, Out](
    val schema: Schema[Repr, A]
  ) {
    def to: G[ReprOut, Out]
  }

  sealed class DerivationTo[G[_, _]] {

    def const[XR, X, ReprOut, Out](
      schema: Schema[XR, X]
    )(
      value: G[ReprOut, Out]
    ): Derivation[G, XR, X, ReprOut, Out] =
      new Derivation[G, XR, X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = value
      }

    //SumCase
    def sum[
      XR,
      YR,
      X,
      Y,
      XROut,
      YROut,
      XOut,
      YOut,
      ReprOut,
      Out
    ](
      schema: Schema[RSum[XR, X, YR, Y], X \/ Y]
    )(
      lDerive: Schema[XR, X] => Derivation[G, XR, X, XROut, XOut],
      rDerive: Schema[YR, Y] => Derivation[G, YR, Y, YROut, YOut]
    )(
      derive: (G[XROut, XOut], G[YROut, YOut]) => G[ReprOut, Out]
    ): Derivation[G, RSum[XR, X, YR, Y], X \/ Y, ReprOut, Out] =
      new Derivation[G, RSum[XR, X, YR, Y], X \/ Y, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val lDerived = lDerive(schema.left)
          val rDerived = rDerive(schema.right)
          derive(lDerived.to, rDerived.to)
        }
      }

    //ProductCase
    def prod[
      XR,
      YR,
      X,
      Y,
      XROut,
      YROut,
      XOut,
      YOut,
      ReprOut,
      Out
    ](
      schema: Schema[RProd[XR, X, YR, Y], (X, Y)]
    )(
      lDerive: Schema[XR, X] => Derivation[G, XR, X, XROut, XOut],
      rDerive: Schema[YR, Y] => Derivation[G, YR, Y, YROut, YOut]
    )(
      derive: (G[XROut, XOut], G[YROut, YOut]) => G[ReprOut, Out]
    ): Derivation[G, RProd[XR, X, YR, Y], (X, Y), ReprOut, Out] =
      new Derivation[G, RProd[XR, X, YR, Y], (X, Y), ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val lDerived = lDerive(schema.left)
          val rDerived = rDerive(schema.right)
          derive(lDerived.to, rDerived.to)
        }
      }

    //field case
    def field[
      I <: R.ProductTermId,
      XR,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[I -*> XR, X]
    )(
      baseDerive: Schema[XR, X] => Derivation[G, XR, X, XROut, XOut]
    )(
      derive: (R.ProductTermId, G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, I -*> XR, X, ReprOut, Out] =
      new Derivation[G, I -*> XR, X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.base)
          derive(schema.id, baseDerived.to)
        }
      }

    //branch case
    def branch[
      I <: R.SumTermId,
      XR,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[I -+> XR, X]
    )(
      baseDerive: Schema[XR, X] => Derivation[G, XR, X, XROut, XOut]
    )(
      derive: (R.SumTermId, G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, I -+> XR, X, ReprOut, Out] =
      new Derivation[G, I -+> XR, X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.base)
          derive(schema.id, baseDerived.to)
        }
      }

    //record case
    def rec[
      XR,
      XP,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[RRecord[XR, XP, X], X]
    )(
      baseDerive: Schema[XR, XP] => Derivation[G, XR, XP, XROut, XOut]
    )(
      derive: (Iso[XP, X], G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, RRecord[XR, XP, X], X, ReprOut, Out] =
      new Derivation[G, RRecord[XR, XP, X], X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.fields)
          derive(schema.iso, baseDerived.to)
        }
      }

    //union case
    def union[
      XR,
      XP,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[RUnion[XR, XP, X], X]
    )(
      baseDerive: Schema[XR, XP] => Derivation[G, XR, XP, XROut, XOut]
    )(
      derive: (Iso[XP, X], G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, RUnion[XR, XP, X], X, ReprOut, Out] =
      new Derivation[G, RUnion[XR, XP, X], X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.choices)
          derive(schema.iso, baseDerived.to)
        }
      }

    //iso case
    def iso[
      XR,
      XP,
      X,
      XROut,
      XOut,
      ReprOut,
      Out
    ](
      schema: Schema[RIso[XR, XP, X], X]
    )(
      baseDerive: Schema[XR, XP] => Derivation[G, XR, XP, XROut, XOut]
    )(
      derive: (Iso[XP, X], G[XROut, XOut]) => G[ReprOut, Out]
    ): Derivation[G, RIso[XR, XP, X], X, ReprOut, Out] =
      new Derivation[G, RIso[XR, XP, X], X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] = {
          val baseDerived = baseDerive(schema.base)
          derive(schema.iso, baseDerived.to)
        }
      }

    //prim case
    def prim[
      X,
      ReprOut,
      Out
    ](
      schema: Schema[R.Prim[X], X]
    )(
      derive: R.Prim[X] => G[ReprOut, Out]
    ): Derivation[G, R.Prim[X], X, ReprOut, Out] =
      new Derivation[G, R.Prim[X], X, ReprOut, Out](schema) {
        override def to: G[ReprOut, Out] =
          derive(schema.prim)
      }

  }

  object DerivationTo {
    def apply[G[_, _]] = new DerivationTo[G] {}
  }

  val migrate = DerivationTo[Schema]

  trait AtPath[Repr, A, P <: HList] {
    type RO
    type O

    def select(schema: Schema[Repr, A]): Schema[RO, O]
  }

  trait LowPrioAtPath0 {

    /*protected def ev[S[_,_], R, A, P <: HList, RT, T]: Any => AtPath.Aux[R, A, P, RT, T] =
     (
     _ => new AtPath[R, A, P] {
     type RO = RT
     type O = T

     }
     )*/

    //TODO: Something with the left/right naming is not correct here
    implicit def atSumLeft[
      Repr,
      A,
      RB,
      B,
      P <: HList,
      RT,
      AT
    ](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT]
    ): AtPath.Aux[RSum[RB, B, Repr, A], B \/ A, P, RT, AT] =
      new AtPath[RSum[RB, B, Repr, A], B \/ A, P] {
        override type RO = RT
        override type O  = AT

        override def select(schema: Schema[RSum[RB, B, Repr, A], B \/ A]): Schema[RT, AT] = {
          val left = schema.unFix.asInstanceOf[Sum[Schema, RB, Repr, B, A]].right
          rest.select(left)
        }
      }

    implicit def atProdLeft[Repr, A, RB, B, P <: HList, RT, AT](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT]
    ): AtPath.Aux[RProd[RB, B, Repr, A], (B, A), P, RT, AT] =
      new AtPath[RProd[RB, B, Repr, A], (B, A), P] {
        override type RO = RT
        override type O  = AT

        override def select(schema: Schema[RProd[RB, B, Repr, A], (B, A)]): Schema[RT, AT] = {
          val left = schema.unFix.asInstanceOf[Prod[Schema, RB, Repr, B, A]].right
          rest.select(left)
        }

      }
  }

  trait LowPrioAtPath extends LowPrioAtPath0 {
    implicit def atField[N <: R.ProductTermId, Repr, A, T <: HList, RT, AT](
      implicit rest: AtPath.Aux[Repr, A, T, RT, AT]
    ): AtPath.Aux[N -*> Repr, A, N :: T, RT, AT] = new AtPath[N -*> Repr, A, N :: T] {
      override type RO = RT
      override type O  = AT

      override def select(schema: Schema[N -*> Repr, A]): Schema[RT, AT] = {
        val inner = schema.unFix.asInstanceOf[Field[Schema, Repr, N, A]].schema
        rest.select(inner)
      }
    }

    implicit def atBranch[N <: R.SumTermId, Repr, A, T <: HList, RT, AT](
      implicit rest: AtPath.Aux[Repr, A, T, RT, AT]
    ): AtPath.Aux[N -+> Repr, A, N :: T, RT, AT] = new AtPath[N -+> Repr, A, N :: T] {
      override type RO = RT
      override type O  = AT

      override def select(schema: Schema[N -+> Repr, A]): Schema[RT, AT] = {
        val inner = schema.unFix.asInstanceOf[Branch[Schema, Repr, N, A]].schema
        rest.select(inner)
      }
    }

    implicit def atRecord[Repr, An, A, P <: HList, RT, AT](
      implicit rest: AtPath.Aux[Repr, An, P, RT, AT]
    ): AtPath.Aux[RRecord[Repr, An, A], A, P, RT, AT] = new AtPath[RRecord[Repr, An, A], A, P] {
      override type RO = RT
      override type O  = AT

      override def select(schema: Schema[RRecord[Repr, An, A], A]): Schema[RT, AT] = {
        val inner = schema.unFix.asInstanceOf[Record[Schema, Repr, An, A]].fields
        rest.select(inner)
      }
    }

    implicit def atUnion[Repr, Ae, A, P <: HList, RT, AT](
      implicit rest: AtPath.Aux[Repr, Ae, P, RT, AT]
    ): AtPath.Aux[RUnion[Repr, Ae, A], A, P, RT, AT] = new AtPath[RUnion[Repr, Ae, A], A, P] {
      override type RO = RT
      override type O  = AT

      override def select(schema: Schema[RUnion[Repr, Ae, A], A]): Schema[RT, AT] = {
        val inner = schema.unFix.asInstanceOf[Union[Schema, Repr, Ae, A]].choices
        rest.select(inner)
      }
    }

    implicit def atIso[Repr, A0, A, P <: HList, RT, AT](
      implicit rest: AtPath.Aux[Repr, A0, P, RT, AT]
    ): AtPath.Aux[RIso[Repr, A0, A], A, P, RT, AT] = new AtPath[RIso[Repr, A0, A], A, P] {
      override type RO = RT
      override type O  = AT

      override def select(schema: Schema[RIso[Repr, A0, A], A]): Schema[RT, AT] = {
        val inner = schema.unFix.asInstanceOf[IsoSchema[Schema, Repr, A0, A]].base
        rest.select(inner)
      }
    }

    implicit def atSeq[Repr, A, P <: HList, RT, AT](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT]
    ): AtPath.Aux[RSeq[Repr, A], A, P, RT, AT] = new AtPath[RSeq[Repr, A], A, P] {
      override type RO = RT
      override type O  = AT

      override def select(schema: Schema[RSeq[Repr, A], A]): Schema[RT, AT] = {
        val inner = schema.unFix.asInstanceOf[Sequence[Schema, Repr, A]].element
        rest.select(inner)
      }
    }

    implicit def atSumRight[Repr, A, RB, B, P <: HList, RT, AT](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT]
    ): AtPath.Aux[RSum[Repr, A, RB, B], A \/ B, P, RT, AT] =
      new AtPath[RSum[Repr, A, RB, B], A \/ B, P] {
        override type RO = RT
        override type O  = AT

        override def select(schema: Schema[RSum[Repr, A, RB, B], A \/ B]): Schema[RT, AT] = {
          val left = schema.unFix.asInstanceOf[Sum[Schema, Repr, RB, A, B]].left
          rest.select(left)
        }
      }

    implicit def atProdRight[Repr, A, RB, B, P <: HList, RT, AT](
      implicit rest: AtPath.Aux[Repr, A, P, RT, AT]
    ): AtPath.Aux[RProd[Repr, A, RB, B], (A, B), P, RT, AT] =
      new AtPath[RProd[Repr, A, RB, B], (A, B), P] {
        override type RO = RT
        override type O  = AT

        override def select(schema: Schema[RProd[Repr, A, RB, B], (A, B)]): Schema[RT, AT] = {
          val left = schema.unFix.asInstanceOf[Prod[Schema, Repr, RB, A, B]].left
          rest.select(left)
        }

      }
  }

  object AtPath extends LowPrioAtPath {

    type Aux[Repr, A, P <: HList, RT, AT] = AtPath[Repr, A, P] {
      type RO = RT
      type O  = AT
    }

    def apply[Repr, A, P <: HList, RT, AT](schema: Schema[Repr, A], path: P)(
      implicit atPath: Aux[Repr, A, P, RT, AT]
    ): Aux[Repr, A, P, RT, AT] = {
      identity(schema)
      identity(path)
      atPath
    }

    implicit def atRoot[Repr, A, P <: HNil]: Aux[Repr, A, P, Repr, A] = new AtPath[Repr, A, P] {
      override type RO = Repr
      override type O  = A

      override def select(schema: Schema[Repr, A]): Schema[Repr, A] = schema
    }

  }

  ////////////////
  // Public API
  ////////////////

  implicit final class SchemaSyntax[Repr, A](schema: Schema[Repr, A]) {

    def :*: [R2, B](left: Schema[R2, B]): Schema[RProd[R2, B, Repr, A], (B, A)] =
      Fix(new ProdF(left, schema))

    def :+: [R2, B](left: Schema[R2, B]): Schema[RSum[R2, B, Repr, A], B \/ A] =
      Fix(new SumF(left, schema))

    def -*>: [I <: R.ProductTermId](
      id: I
    ): Schema[I -*> Repr, A] =
      Fix(FieldF(id, schema))

    def -+>: [I <: R.SumTermId](id: I): Schema[I -+> Repr, A] =
      Fix(BranchF(id, schema))

    def to[F[_, _]](implicit interpreter: RInterpreter[F]): F[_, A] = interpreter.interpret(schema)

    def imap[B](_iso: Iso[A, B]): Schema[RIso[Repr, A, B], B] = Fix(IsoSchemaF(schema, _iso))
    /*
    schema.unFix match {
      case i: IsoSchema[Schema, Repr, a0, A] =>
        Fix(
          IsoSchemaF[Schema, Repr, a0, B, R.Prim, R.SumTermId, R.ProductTermId](
            i.base,
            i.iso.composeIso(_iso)
          )
        )
      case _ => Fix(IsoSchemaF(schema, _iso))
    }
   */
  }

  final def unit: Schema[Unit, Unit] =
    Fix(
      One()
    )

  final def prim[A](prim: R.Prim[A]): Schema[R.Prim[A], A] =
    Fix(
      PrimSchemaF(prim)
    )

  final def union[Repr: IsUnion, A, AE](
    choices: Schema[Repr, AE],
    iso: Iso[AE, A]
  ): Schema[RUnion[Repr, AE, A], A] =
    Fix(UnionF(choices, iso))

  final def optional[Repr, A](
    aSchema: Schema[Repr, A]
  ): Schema[RIso[RSum[Repr, A, Unit, Unit], A \/ Unit, Option[A]], Option[A]] =
    iso(
      Fix(SumF(aSchema, unit)),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[Repr: IsRecord, A, An](
    terms: Schema[Repr, An],
    isoA: Iso[An, A]
  ): Schema[RRecord[Repr, An, A], A] =
    Fix(RecordF(terms, isoA))

  final def seq[Repr, A](element: Schema[Repr, A]): Schema[RSeq[Repr, A], List[A]] =
    Fix(SeqF(element))

  final def iso[Repr, A0, A](
    base: Schema[Repr, A0],
    iso: Iso[A0, A]
  ): Schema[RIso[Repr, A0, A], A] =
    Fix(IsoSchemaF(base, iso))

  final def self[A](root: => Schema[_, A]): Schema[RSelf[A], A] =
    Fix(
      SelfReference(() => root, new (Schema ~~> Schema) { def apply[R0, X](a: Schema[R0, X]) = a })
    )

}
