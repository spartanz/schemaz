package schemaz

//import Scalaz._
import recursion._

import monocle.Iso
import scalaz.{ -\/, \/, \/-, ~> }

import shapeless._

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

object Representation {
  type +[A, B]
  type *[A, B]
  type RIso[RA, A, B]
  type RSelf[A]
  type RSeq[A]
  type RField[K, V]
  type RBranch[K, V]
  type RRecord[RA, An, A]
  type RUnion[RA, An, A]
}

import Representation._

sealed trait SchemaF[Prim[_], SumTermId, ProductTermId, F[_, _], R, A] {
  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, R, A]
}

trait SelfRef {}

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
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RA + RB, A \/ B] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RA + RB, A \/ B] =
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
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RA * RB, (A, B)] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RA * RB, (A, B)] =
    ProdF(nt(left), nt(right))
  override def toString: String = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchemaF[F[_, _], A, Prim[_], SumTermId, ProductTermId](prim: Prim[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, A, A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A, A] =
    PrimSchemaF[G, A, Prim, SumTermId, ProductTermId](prim)
}

/**
 * A named branch of an union
 */
final case class BranchF[F[_, _], RA, I <: SumTermId: Witness.Aux, A, Prim[_], SumTermId, ProductTermId](
  id: I,
  schema: F[RA, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RBranch[I, RA], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RBranch[I, RA], A] =
    BranchF(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 * This class cannot be constructed directly, you must use the `SchemaModule#union` method.
 */
sealed abstract case class UnionF[F[_, _], RA, A, AE, Prim[_], SumTermId, ProductTermId](
  choices: F[RA, AE],
  iso: Iso[AE, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RUnion[RA, AE, A], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RUnion[RA, AE, A], A] =
    new UnionF[G, RA, A, AE, Prim, SumTermId, ProductTermId](nt(choices), iso) {}
}

/**
 * A named field of a record
 */
final case class FieldF[F[_, _], RA, I <: ProductTermId: Witness.Aux, A, Prim[_], SumTermId, ProductTermId](
  id: I,
  schema: F[RA, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RField[I, RA], A] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RField[I, RA], A] =
    FieldF(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 * This class cannot be constructed directly, you must use the `SchemaModule#record` method.
 */
sealed abstract case class RecordF[F[_, _], RA, A, AP, Prim[_], SumTermId, ProductTermId](
  fields: F[RA, AP],
  iso: Iso[AP, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, RRecord[RA, AP, A], A] {

  def hmap[G[_, _]](
    nt: F ~~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, RRecord[RA, AP, A], A] =
    new RecordF[G, RA, A, AP, Prim, SumTermId, ProductTermId](nt(fields), iso) {}
}

/**
 * A sequence
 */
final case class SeqF[F[_, _], RA, A, Prim[_], SumTermId, ProductTermId](element: F[RA, A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, RSeq[RA], List[A]] {

  def hmap[G[_, _]](nt: F ~~> G): SchemaF[Prim, SumTermId, ProductTermId, G, RSeq[RA], List[A]] =
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

  type FSchema[Prim[_], SumTermId, ProductTermId, A] =
    Fix[SchemaF[Prim, SumTermId, ProductTermId, ?[_, _], ?, ?], _, A]

  type FSchemaR[Prim[_], SumTermId, ProductTermId, Repr, A] =
    Fix[SchemaF[Prim, SumTermId, ProductTermId, ?[_, _], ?, ?], Repr, A]

  sealed private[schemaz] trait LabelledSum_[A, Repr, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchemaR[Prim, SumTermId, ProductTermId, Repr, A]

    def :+: [R2, B](
      l: LabelledSum_[B, R2, Prim, SumTermId, ProductTermId]
    ): LabelledSum_[B \/ A, R2 + Repr, Prim, SumTermId, ProductTermId] = LabelledSum2(l, this)
  }

  final private[schemaz] case class LabelledSum1[A, Repr, I <: SumTermId: Witness.Aux, Prim[_], SumTermId, ProductTermId](
    id: I,
    schema: FSchemaR[Prim, SumTermId, ProductTermId, Repr, A]
  ) extends LabelledSum_[A, RBranch[I, Repr], Prim, SumTermId, ProductTermId] {

    def toSchema =
      Fix(BranchF(id, schema))

  }

  final private[schemaz] case class LabelledSum2[A, B, R1, R2, Prim[_], SumTermId, ProductTermId](
    l: LabelledSum_[A, R1, Prim, SumTermId, ProductTermId],
    r: LabelledSum_[B, R2, Prim, SumTermId, ProductTermId]
  ) extends LabelledSum_[A \/ B, R1 + R2, Prim, SumTermId, ProductTermId] {
    def toSchema = Fix(new SumF(l.toSchema, r.toSchema))

  }

  sealed private[schemaz] trait LabelledProduct_[A, Repr, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchemaR[Prim, SumTermId, ProductTermId, Repr, A]

    def :*: [R2, B](
      l: LabelledProduct_[B, R2, Prim, SumTermId, ProductTermId]
    ): LabelledProduct_[(B, A), R2 * Repr, Prim, SumTermId, ProductTermId] =
      LabelledProduct2(l, this)
  }

  final private[schemaz] case class LabelledProduct1[A, Repr, I <: ProductTermId: Witness.Aux, Prim[
    _
  ], SumTermId, ProductTermId](
    id: I,
    schema: FSchemaR[Prim, SumTermId, ProductTermId, Repr, A]
  ) extends LabelledProduct_[A, RField[I, Repr], Prim, SumTermId, ProductTermId] {

    def toSchema =
      Fix(
        FieldF(
          id,
          schema
        )
      )

  }

  final private[schemaz] case class LabelledProduct2[A, B, R1, R2, Prim[_], SumTermId, ProductTermId](
    l: LabelledProduct_[A, R1, Prim, SumTermId, ProductTermId],
    r: LabelledProduct_[B, R2, Prim, SumTermId, ProductTermId]
  ) extends LabelledProduct_[(A, B), R1 * R2, Prim, SumTermId, ProductTermId] {
    def toSchema = Fix(new ProdF(l.toSchema, r.toSchema))

  }

}

trait SchemaModule[R <: Realisation] {

  val R: R

  import SchemaF._

  type RInterpreter[F[_, _]] = Interpreter[Schema, F]

  type RSchema[F[_, _], Repr, A] = SchemaF[R.Prim, R.SumTermId, R.ProductTermId, F, Repr, A]

  type BareSchema[A] = Fix[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_, _], ?, ?], _, A]

  type Schema[Repr, A] = Fix[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_, _], ?, ?], Repr, A]

  type Schema_[A] = BareSchema[A]

  type LabelledSum[Repr, A] = LabelledSum_[A, Repr, R.Prim, R.SumTermId, R.ProductTermId]

  type LabelledProduct[Repr, A] = LabelledProduct_[A, Repr, R.Prim, R.SumTermId, R.ProductTermId]

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

  ////////////////
  // Public API
  ////////////////

  implicit final class SchemaSyntax[Repr, A](schema: Schema[Repr, A]) {

    def :*: [R2, B](left: Schema[R2, B]): Schema[R2 * Repr, (B, A)] =
      Fix(new ProdF(left, schema))

    def :+: [R2, B](left: Schema[R2, B]): Schema[R2 + Repr, B \/ A] =
      Fix(new SumF(left, schema))

    def -*>: [I <: R.ProductTermId: Witness.Aux](id: I): LabelledProduct[RField[I, Repr], A] =
      LabelledProduct1(id, schema)

    def -+>: [I <: R.SumTermId: Witness.Aux](id: I): LabelledSum[RBranch[I, Repr], A] =
      LabelledSum1(id, schema)

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

  final def prim[A](prim: R.Prim[A]): Schema[A, A] =
    Fix(
      PrimSchemaF(prim)
    )

  final def union[Repr, A, AE](
    choices: LabelledSum[Repr, AE],
    iso: Iso[AE, A]
  ): Schema[RUnion[Repr, AE, A], A] =
    Fix(
      new UnionF[
        FSchemaR[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        Repr,
        A,
        AE,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](choices.toSchema, iso) {}
    )

  final def optional[Repr, A](
    aSchema: Schema[Repr, A]
  ): Schema[RIso[Repr + Unit, A \/ Unit, Option[A]], Option[A]] =
    iso(
      Fix(SumF(aSchema, unit)),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[Repr, A, An](
    terms: LabelledProduct[Repr, An],
    isoA: Iso[An, A]
  ): Schema[RRecord[Repr, An, A], A] =
    Fix(
      new RecordF[
        FSchemaR[R.Prim, R.SumTermId, R.ProductTermId, ?, ?],
        Repr,
        A,
        An,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](terms.toSchema, isoA) {}
    )

  final def seq[Repr, A](element: Schema[Repr, A]): Schema[RSeq[Repr], List[A]] =
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
