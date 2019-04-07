package scalaz

package schema

import recursion._

import monocle.Iso

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

sealed trait SchemaF[Prim[_], SumTermId, ProductTermId, F[_], A] {
  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A]
}

trait SelfRef        {}
trait FieldR[A]      {}
trait BranchR[A]     {}
trait UnionR[AE, A]  {}
trait RecordR[AP, A] {}
trait IsoR[A, B]     {}

////////////////////
// The Schema ADT
////////////////////

// "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

final case class One[F[_], Prim[_], SumTermId, ProductTermId]()
    extends SchemaF[Prim, SumTermId, ProductTermId, F, Unit] {
  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, Unit] = One()
}

/**
 * The sum of two schemas, yielding the schema for `A \/ B`
 */
final case class SumF[F[_], A, B, Prim[_], SumTermId, ProductTermId](left: F[A], right: F[B])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, A \/ B] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A \/ B] =
    SumF(nt(left), nt(right))
  override def toString: String = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class ProdF[F[_], A, B, Prim[_], SumTermId, ProductTermId](left: F[A], right: F[B])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, (A, B)] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, (A, B)] =
    ProdF(nt(left), nt(right))
  override def toString: String = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchemaF[F[_], A, Prim[_], SumTermId, ProductTermId](prim: Prim[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    PrimSchemaF[G, A, Prim, SumTermId, ProductTermId](prim)
}

/**
 * A named branch of an union
 */
final case class BranchF[F[_], A, Prim[_], SumTermId, ProductTermId](id: SumTermId, schema: F[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    BranchF(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 * This class cannot be constructed directly, you must use the `SchemaModule#union` method.
 */
sealed abstract case class UnionF[F[_], A, AE, Prim[_], SumTermId, ProductTermId](
  choices: F[AE],
  iso: Iso[AE, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    new UnionF[G, A, AE, Prim, SumTermId, ProductTermId](nt(choices), iso) {}
}

/**
 * A named field of a record
 */
final case class FieldF[F[_], A, Prim[_], SumTermId, ProductTermId](
  id: ProductTermId,
  schema: F[A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    FieldF(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 * This class cannot be constructed directly, you must use the `SchemaModule#record` method.
 */
/*sealed*/
abstract case class RecordF[F[_], A, AP, Prim[_], SumTermId, ProductTermId](
  fields: F[AP],
  iso: Iso[AP, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    new RecordF[G, A, AP, Prim, SumTermId, ProductTermId](nt(fields), iso) {}
}

/**
 * A sequence
 */
final case class SeqF[F[_], A, Prim[_], SumTermId, ProductTermId](element: F[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, List[A]] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, List[A]] =
    SeqF(nt(element))
}

/**
 * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
 * between AO and A, then a schema of A0 can be used to represent values of A.
 */
final case class IsoSchemaF[F[_], A0, A, Prim[_], SumTermId, ProductTermId](
  base: F[A0],
  iso: Iso[A0, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    IsoSchemaF(nt(base), iso)
}

final case class SelfReference[F[_], H[_], A, Prim[_], SumTermId, ProductTermId](
  private val ref: () => F[A],
  private val nattrans: F ~> H
) extends SchemaF[Prim, SumTermId, ProductTermId, H, A] {

  lazy val unroll: H[A] = nattrans(ref())

  def hmap[G[_]](nt: H ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    SelfReference[F, G, A, Prim, SumTermId, ProductTermId](ref, nt.compose(nattrans))
}

/**
 * An interpreter able to derive a `F[A]` from a schema for `A` (for any `A`).
 * Such interpreters will usually be implemented using a recursion scheme like
 * 'cataNT`or hyloNT`.
 */
trait Interpreter[F[_], G[_]] { self =>

  /**
   * A natural transformation that will transform a schema for any type `A`
   * into an `F[A]`.
   */
  def interpret: F ~> G

  def compose[H[_]](nt: H ~> F) = self match {
    case i: ComposedInterpreter[h, G, F] => ComposedInterpreter(i.underlying, i.nt.compose(nt))
    case x                               => ComposedInterpreter(x, nt)
  }
}

final case class ComposedInterpreter[F[_], G[_], H[_]](underlying: Interpreter[F, G], nt: H ~> F)
    extends Interpreter[H, G] {
  final override val interpret = underlying.interpret.compose(nt)
}

class CataInterpreter[S[_[_], _], F[_]](
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[Fix[S, ?], F] {
  final override val interpret = cataNT(algebra)
}

class HyloInterpreter[S[_[_], _], F[_], G[_]](
  coalgebra: HCoalgebra[S, G],
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[G, F] {
  final override val interpret = hyloNT(coalgebra, algebra)
}

object SchemaF {

  implicit def schemaHFunctor[Prim[_], SumTermId, ProductTermId] =
    new HFunctor[SchemaF[Prim, SumTermId, ProductTermId, ?[_], ?]] {

      def hmap[F[_], G[_]](nt: F ~> G) =
        new (SchemaF[Prim, SumTermId, ProductTermId, F, ?] ~> SchemaF[
          Prim,
          SumTermId,
          ProductTermId,
          G,
          ?
        ]) {
          def apply[A](fa: SchemaF[Prim, SumTermId, ProductTermId, F, A]) = fa.hmap(nt)
        }
    }

  type FSchema[Prim[_], SumTermId, ProductTermId, A] =
    Fix[SchemaF[Prim, SumTermId, ProductTermId, ?[_], ?], A]

  type FSchemaR[Prim[_], SumTermId, ProductTermId, A, Repr] =
    FixR.Aux[SchemaF[Prim, SumTermId, ProductTermId, ?[_], ?], Repr, A]

  sealed private[schema] trait LabelledSum_[A, Repr, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchemaR[Prim, SumTermId, ProductTermId, A, Repr]

    def :+: [R2, B](
      l: LabelledSum_[B, R2, Prim, SumTermId, ProductTermId]
    ): LabelledSum_[B \/ A, R2 \/ Repr, Prim, SumTermId, ProductTermId] = LabelledSum2(l, this)
  }

  final private[schema] case class LabelledSum1[A, Repr, Prim[_], SumTermId, ProductTermId](
    id: SumTermId,
    schema: FSchemaR[Prim, SumTermId, ProductTermId, A, Repr]
  ) extends LabelledSum_[A, BranchR[Repr], Prim, SumTermId, ProductTermId] {

    def toSchema =
      FixR[BranchR[Repr]](
        BranchF[FSchema[Prim, SumTermId, ProductTermId, ?], A, Prim, SumTermId, ProductTermId](
          id,
          schema.toFix
        )
      )

  }

  final private[schema] case class LabelledSum2[A, B, R1, R2, Prim[_], SumTermId, ProductTermId](
    l: LabelledSum_[A, R1, Prim, SumTermId, ProductTermId],
    r: LabelledSum_[B, R2, Prim, SumTermId, ProductTermId]
  ) extends LabelledSum_[A \/ B, R1 \/ R2, Prim, SumTermId, ProductTermId] {
    def toSchema = FixR[R1 \/ R2](new SumF(l.toSchema.toFix, r.toSchema.toFix))

  }

  sealed private[schema] trait LabelledProduct_[A, Repr, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchemaR[Prim, SumTermId, ProductTermId, A, Repr]

    def :*: [R2, B](
      l: LabelledProduct_[B, R2, Prim, SumTermId, ProductTermId]
    ): LabelledProduct_[(B, A), (R2, Repr), Prim, SumTermId, ProductTermId] =
      LabelledProduct2(l, this)
  }

  final private[schema] case class LabelledProduct1[A, Repr, Prim[_], SumTermId, ProductTermId](
    id: ProductTermId,
    schema: FSchemaR[Prim, SumTermId, ProductTermId, A, Repr]
  ) extends LabelledProduct_[A, FieldR[Repr], Prim, SumTermId, ProductTermId] {

    def toSchema =
      FixR[FieldR[Repr]](
        FieldF[FSchema[Prim, SumTermId, ProductTermId, ?], A, Prim, SumTermId, ProductTermId](
          id,
          schema.toFix
        )
      )

  }

  final private[schema] case class LabelledProduct2[A, B, R1, R2, Prim[_], SumTermId, ProductTermId](
    l: LabelledProduct_[A, R1, Prim, SumTermId, ProductTermId],
    r: LabelledProduct_[B, R2, Prim, SumTermId, ProductTermId]
  ) extends LabelledProduct_[(A, B), (R1, R2), Prim, SumTermId, ProductTermId] {
    def toSchema = FixR[(R1, R2)](new ProdF(l.toSchema.toFix, r.toSchema.toFix))

  }
}

trait SchemaModule[R <: Realisation] {

  val R: R

  import SchemaF._

  type RInterpreter[F[_]] = Interpreter[BareSchema, F]

  type RSchema[F[_], A] = SchemaF[R.Prim, R.SumTermId, R.ProductTermId, F, A]

  type BareSchema[A] = Fix[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], A]

  type Schema[Repr, A] = FixR.Aux[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], Repr, A]

  type Schema_[A] = FixR[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], A]

  type LabelledSum[Repr, A] = LabelledSum_[A, Repr, R.Prim, R.SumTermId, R.ProductTermId]

  type LabelledProduct[Repr, A] = LabelledProduct_[A, Repr, R.Prim, R.SumTermId, R.ProductTermId]

  type ROne[F[_]]            = One[F, R.Prim, R.SumTermId, R.ProductTermId]
  type Sum[F[_], A, B]       = SumF[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Prod[F[_], A, B]      = ProdF[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Branch[F[_], A]       = BranchF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Union[F[_], AE, A]    = UnionF[F, A, AE, R.Prim, R.SumTermId, R.ProductTermId]
  type Field[F[_], A]        = FieldF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Record[F[_], An, A]   = RecordF[F, A, An, R.Prim, R.SumTermId, R.ProductTermId]
  type Sequence[F[_], A]     = SeqF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type IsoSchema[F[_], A, B] = IsoSchemaF[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type PrimSchema[F[_], A]   = PrimSchemaF[F, A, R.Prim, R.SumTermId, R.ProductTermId]

  trait Derivation[Repr, A, G[_, _]] {
    type ReprOut
    type Out
    def derive(schema: Schema[Repr, A]): G[ReprOut, Out]
  }

  object Derivation {

    type Aux[Repr, A, G[_, _], ReprOut0, Out0] = Derivation[Repr, A, G] {
      type ReprOut = ReprOut0
      type Out     = Out0
    }

    final case class Deriver[Repr, A](schema: Schema[Repr, A]) {
      def deriveTo[G[_, _]](implicit derivation: Derivation[Repr, A, G]) = derivation.derive(schema)
    }

    sealed abstract class Step[T, T1] {
      def apply(t: T): T1
    }
    sealed class UnitStep[G[_, _], XR, X](unit: () => G[XR, X]) extends Step[Unit, G[XR, X]] {
      def apply(t: Unit): G[XR, X] = unit()
    }

    sealed class PrimStep[G[_, _], A, XR, X](f: R.Prim[A] => G[XR, X])
        extends Step[R.Prim[A], G[XR, X]] {
      def apply(t: R.Prim[A]): G[XR, X] = f(t)
    }

    sealed class ProdStep[G[_, _], XR, X, YR, Y, ZR, Z](f: ((G[XR, X], G[YR, Y])) => G[ZR, Z])
        extends Step[(G[XR, X], G[YR, Y]), G[ZR, Z]] {
      def apply(t: (G[XR, X], G[YR, Y])): G[ZR, Z] = f(t)
    }
    sealed class SumStep[G[_, _], XR, X, YR, Y, ZR, Z](f: ((G[XR, X], G[YR, Y])) => G[ZR, Z])
        extends Step[(G[XR, X], G[YR, Y]), G[ZR, Z]] {
      def apply(t: (G[XR, X], G[YR, Y])): G[ZR, Z] = f(t)
    }

    sealed class BranchStep[G[_, _], XR, X, YR, Y](f: ((R.SumTermId, G[XR, X])) => G[YR, Y])
        extends Step[((R.SumTermId, G[XR, X])), G[YR, Y]] {
      def apply(t: (R.SumTermId, G[XR, X])): G[YR, Y] = f(t)
    }

    sealed class UnionStep[G[_, _], XEOrig, XOrig, XR, X, YR, Y](
      f: ((Iso[XEOrig, XOrig], G[XR, X])) => G[YR, Y]
    ) extends Step[(Iso[XEOrig, XOrig], G[XR, X]), G[YR, Y]] {
      def apply(t: (Iso[XEOrig, XOrig], G[XR, X])): G[YR, Y] = f(t)
    }

    sealed class FieldStep[G[_, _], XR, X, YR, Y](f: ((R.ProductTermId, G[XR, X])) => G[YR, Y])
        extends Step[((R.ProductTermId, G[XR, X])), G[YR, Y]] {
      def apply(t: (R.ProductTermId, G[XR, X])): G[YR, Y] = f(t)
    }

    sealed class RecordStep[G[_, _], XEOrig, XOrig, XR, X, YR, Y](
      f: ((Iso[XEOrig, XOrig], G[XR, X])) => G[YR, Y]
    ) extends Step[(Iso[XEOrig, XOrig], G[XR, X]), G[YR, Y]] {
      def apply(t: (Iso[XEOrig, XOrig], G[XR, X])): G[YR, Y] = f(t)
    }

    sealed class IsoStep[G[_, _], XEOrig, XOrig, XR, X, YR, Y](
      f: ((Iso[XEOrig, XOrig], G[XR, X])) => G[YR, Y]
    ) extends Step[(Iso[XEOrig, XOrig], G[XR, X]), G[YR, Y]] {
      def apply(t: (Iso[XEOrig, XOrig], G[XR, X])): G[YR, Y] = f(t)
    }

    implicit def unitDerivation[
      G[_, _],
      ReprOut0,
      Out0
    ](
      implicit
      step: UnitStep[G, ReprOut0, Out0]
    ): Derivation.Aux[Unit, Unit, G, ReprOut0, Out0] = new Derivation[Unit, Unit, G] {
      override type ReprOut = ReprOut0
      override type Out     = Out0

      override def derive(schema: Schema[Unit, Unit]): G[ReprOut0, Out0] = step(())
    }

    implicit def primDerivation[
      A,
      G[_, _],
      ReprOut0,
      Out0
    ](
      implicit
      step: PrimStep[G, A, ReprOut0, Out0]
    ): Derivation.Aux[R.Prim[A], A, G, ReprOut0, Out0] = new Derivation[R.Prim[A], A, G] {
      override type ReprOut = ReprOut0
      override type Out     = Out0

      override def derive(schema: Schema[R.Prim[A], A]): G[ReprOut0, Out0] =
        step(schema.unFix.asInstanceOf[PrimSchema[Schema_, A]].prim)
    }

    implicit def prodDerivation[
      XRepr,
      X,
      XReprOut,
      XOut,
      YRepr,
      Y,
      YReprOut,
      YOut,
      G[_, _],
      ReprOut0,
      Out0
    ](
      implicit
      lDerive: Derivation.Aux[XRepr, X, G, XReprOut, XOut],
      rDerive: Derivation.Aux[YRepr, Y, G, YReprOut, YOut],
      step: ProdStep[G, XReprOut, XOut, YReprOut, YOut, ReprOut0, Out0]
    ): Derivation.Aux[(XRepr, YRepr), (X, Y), G, ReprOut0, Out0] =
      new Derivation[(XRepr, YRepr), (X, Y), G] {
        override type ReprOut = ReprOut0
        override type Out     = Out0

        override def derive(schema: Schema[(XRepr, YRepr), (X, Y)]): G[ReprOut0, Out0] = {
          val schemaF  = schema.unFix.asInstanceOf[Prod[Schema_, X, Y]]
          val lDerived = lDerive.derive(schemaF.left.asInstanceOf[Schema[XRepr, X]])
          val rDerived = rDerive.derive(schemaF.right.asInstanceOf[Schema[YRepr, Y]])
          step((lDerived, rDerived))
        }
      }

    implicit def sumDerivation[
      XRepr,
      X,
      XReprOut,
      XOut,
      YRepr,
      Y,
      YReprOut,
      YOut,
      G[_, _],
      ReprOut0,
      Out0
    ](
      implicit
      lDerive: Derivation.Aux[XRepr, X, G, XReprOut, XOut],
      rDerive: Derivation.Aux[YRepr, Y, G, YReprOut, YOut],
      step: SumStep[G, XReprOut, XOut, YReprOut, YOut, ReprOut0, Out0]
    ): Derivation.Aux[XRepr \/ YRepr, X \/ Y, G, ReprOut0, Out0] =
      new Derivation[XRepr \/ YRepr, X \/ Y, G] {
        override type ReprOut = ReprOut0
        override type Out     = Out0

        override def derive(schema: Schema[XRepr \/ YRepr, X \/ Y]): G[ReprOut0, Out0] = {
          val schemaF  = schema.unFix.asInstanceOf[Sum[Schema_, X, Y]]
          val lDerived = lDerive.derive(schemaF.left.asInstanceOf[Schema[XRepr, X]])
          val rDerived = rDerive.derive(schemaF.right.asInstanceOf[Schema[YRepr, Y]])
          step((lDerived, rDerived))
        }
      }

    implicit def branchDerivation[
      XRepr,
      X,
      XReprOut,
      XOut,
      G[_, _],
      ReprOut0,
      Out0
    ](
      implicit
      baseDerive: Derivation.Aux[XRepr, X, G, XReprOut, XOut],
      step: BranchStep[G, XReprOut, XOut, ReprOut0, Out0]
    ): Derivation.Aux[BranchR[XRepr], X, G, ReprOut0, Out0] =
      new Derivation[BranchR[XRepr], X, G] {
        override type ReprOut = ReprOut0
        override type Out     = Out0

        override def derive(schema: Schema[BranchR[XRepr], X]): G[ReprOut0, Out0] = {
          val schemaF = schema.unFix.asInstanceOf[Branch[Schema_, X]]
          val inner   = baseDerive.derive(schemaF.schema.asInstanceOf[Schema[XRepr, X]])
          step((schemaF.id, inner))
        }
      }

    implicit def unionDerivation[
      XRepr,
      XE,
      X,
      XReprOut,
      XOut,
      G[_, _],
      ReprOut0,
      Out0
    ](
      implicit
      baseDerive: Derivation.Aux[XRepr, XE, G, XReprOut, XOut],
      step: UnionStep[G, XE, X, XReprOut, XOut, ReprOut0, Out0]
    ): Derivation.Aux[UnionR[XE, X], X, G, ReprOut0, Out0] =
      new Derivation[UnionR[XE, X], X, G] {
        override type ReprOut = ReprOut0
        override type Out     = Out0

        override def derive(schema: Schema[UnionR[XE, X], X]): G[ReprOut0, Out0] = {
          val schemaF = schema.unFix.asInstanceOf[Union[Schema_, XE, X]]
          val inner   = baseDerive.derive(schemaF.choices.asInstanceOf[Schema[XRepr, XE]])
          step((schemaF.iso, inner))
        }
      }

    implicit def fieldDerivation[
      XRepr,
      X,
      XReprOut,
      XOut,
      G[_, _],
      ReprOut0,
      Out0
    ](
      implicit
      baseDerive: Derivation.Aux[XRepr, X, G, XReprOut, XOut],
      step: FieldStep[G, XReprOut, XOut, ReprOut0, Out0]
    ): Derivation.Aux[FieldR[XRepr], X, G, ReprOut0, Out0] =
      new Derivation[FieldR[XRepr], X, G] {
        override type ReprOut = ReprOut0
        override type Out     = Out0

        override def derive(schema: Schema[FieldR[XRepr], X]): G[ReprOut0, Out0] = {
          val schemaF = schema.unFix.asInstanceOf[Field[Schema_, X]]
          val inner   = baseDerive.derive(schemaF.schema.asInstanceOf[Schema[XRepr, X]])
          step((schemaF.id, inner))
        }
      }

    implicit def recordDerivation[
      XRepr,
      XE,
      X,
      XReprOut,
      XOut,
      G[_, _],
      ReprOut0,
      Out0
    ](
      implicit
      baseDerive: Derivation.Aux[XRepr, XE, G, XReprOut, XOut],
      step: RecordStep[G, XE, X, XReprOut, XOut, ReprOut0, Out0]
    ): Derivation.Aux[RecordR[XE, X], X, G, ReprOut0, Out0] =
      new Derivation[RecordR[XE, X], X, G] {
        override type ReprOut = ReprOut0
        override type Out     = Out0

        override def derive(schema: Schema[RecordR[XE, X], X]): G[ReprOut0, Out0] = {
          val schemaF = schema.unFix.asInstanceOf[Record[Schema_, XE, X]]
          val inner   = baseDerive.derive(schemaF.fields.asInstanceOf[Schema[XRepr, XE]])
          step((schemaF.iso, inner))
        }
      }
    /*   implicit def isoDerivation[
      XRepr,
      XE,
      X,
      XReprOut,
      XOut,
      G[_,_],
      ReprOut0,
      Out0
    ](
      implicit
        baseDerive:Derivation.Aux[XRepr, XE, G, XReprOut, XOut],
      step:IsoStep[G, XE, X, XReprOut, XOut, ReprOut0, Out0]
    ):Derivation.Aux[IsoR[XE,X], X, G, ReprOut0, Out0]  =
      new Derivation[IsoR[XE,X], X, G] {
        override type ReprOut = ReprOut0
        override type Out = Out0

        override def derive(schema: Schema[IsoR[XE,X], X]):G[ReprOut0, Out0] = {
          val schemaF = schema.unFix.asInstanceOf[IsoSchema[Schema_, XE, X]]
          val inner = baseDerive.derive(schemaF.base.asInstanceOf[Schema[XRepr,XE]])
          step((schemaF.iso, inner))
        }
      }*/
  }

  object Interpreter {

    def cata[S[_[_], _], F[_]](alg: HAlgebra[S, F])(implicit ev: HFunctor[S]) =
      new CataInterpreter[S, F](alg)

    def hylo[S[_[_], _], F[_], G[_]](coalg: HCoalgebra[S, G], alg: HAlgebra[S, F])(
      implicit ev: HFunctor[S]
    ) = new HyloInterpreter(coalg, alg)

  }

  ////////////////
  // Public API
  ////////////////

  implicit final class SchemaSyntax[Repr, A](schema: Schema[Repr, A]) {

    def :*: [R2, B](left: Schema[R2, B]): Schema[(R2, Repr), (B, A)] =
      FixR[(R2, Repr)](new ProdF(left.toFix, schema.toFix))

    def :+: [R2, B](left: Schema[R2, B]): Schema[R2 \/ Repr, B \/ A] =
      FixR[R2 \/ Repr](new SumF(left.toFix, schema.toFix))

    def -*>: (id: R.ProductTermId): LabelledProduct[FieldR[Repr], A] = LabelledProduct1(id, schema)

    def -+>: (id: R.SumTermId): LabelledSum[BranchR[Repr], A] = LabelledSum1(id, schema)

    def to[F[_]](implicit interpreter: RInterpreter[F]): F[A] = interpreter.interpret(schema.toFix)

    def imap[B](_iso: Iso[A, B]): Schema[Repr, B] = schema.unFix match {
      case IsoSchemaF(base, iso) => FixR[Repr](IsoSchemaF(base, iso.composeIso(_iso)))
      case _                     => FixR[Repr](IsoSchemaF(schema.toFix, _iso))
    }

  }

  final def unit: Schema[Unit, Unit] =
    FixR[Unit](
      One[FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?], R.Prim, R.SumTermId, R.ProductTermId]()
    )

  final def prim[A](prim: R.Prim[A]): Schema[R.Prim[A], A] =
    FixR[R.Prim[A]](
      PrimSchemaF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](prim)
    )

  final def union[Repr, A, AE](
    choices: LabelledSum[Repr, AE],
    iso: Iso[AE, A]
  ): Schema[UnionR[Repr, A], A] =
    FixR[UnionR[Repr, A]](
      new UnionF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        AE,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](choices.toSchema.toFix, iso) {}
    )

  final def optional[Repr, A](
    aSchema: Schema[Repr, A]
  ): Schema[IsoR[Repr \/ Unit, Option[A]], Option[A]] =
    iso[Repr \/ Unit, A \/ Unit, Option[A]](
      FixR[Repr \/ Unit](
        new SumF[
          FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
          A,
          Unit,
          R.Prim,
          R.SumTermId,
          R.ProductTermId
        ](aSchema.toFix, unit.toFix)
      ),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[Repr, A, An](
    terms: LabelledProduct[Repr, An],
    isoA: Iso[An, A]
  ): Schema[RecordR[Repr, A], A] =
    FixR[RecordR[Repr, A]](
      new RecordF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        An,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](terms.toSchema.toFix, isoA) {}
    )

  final def seq[Repr, A](element: Schema[Repr, A]): Schema[Repr, List[A]] =
    FixR[Repr](
      SeqF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](element.toFix)
    )

  final def iso[Repr, A0, A](base: Schema[Repr, A0], iso: Iso[A0, A]): Schema[IsoR[Repr, A], A] =
    FixR[IsoR[Repr, A]](IsoSchemaF(base.toFix, iso))

  final def self[A](root: => Schema[_, A]): Schema[SelfRef, A] =
    FixR[SelfRef](SelfReference(() => root.toFix, λ[BareSchema ~> BareSchema](a => a)))
}
