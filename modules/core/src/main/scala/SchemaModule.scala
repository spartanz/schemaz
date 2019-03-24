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
  type L
  type R
  def inLeft: Option[F[L]]                                                      = None
  def withLeft(f: F[L] => F[L]): SchemaF[Prim, SumTermId, ProductTermId, F, A]  = this
  def withRight(f: F[R] => F[R]): SchemaF[Prim, SumTermId, ProductTermId, F, A] = this
  //def putLeft(fl: F[L]): this.type  = this
  def inRight: Option[F[R]] = None
  //def putRight(fr: F[R]): this.type = this
  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A]
}

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

  override type L = A
  override type R = B

  override def withLeft(f: F[A] => F[A]): SumF[F, A, B, Prim, SumTermId, ProductTermId] =
    SumF(f(left), right)
  override def withRight(f: F[B] => F[B]): SumF[F, A, B, Prim, SumTermId, ProductTermId] =
    SumF(left, f(right))

  override def inLeft: Option[F[L]]  = Some(left)
  override def inRight: Option[F[R]] = Some(right)

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A \/ B] =
    SumF(nt(left), nt(right))
  override def toString: String = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class ProdF[F[_], A, B, Prim[_], SumTermId, ProductTermId](left: F[A], right: F[B])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, (A, B)] {

  override type L = A
  override type R = B

  override def inLeft: Option[F[L]]  = Some(left)
  override def inRight: Option[F[R]] = Some(right)

  override def withLeft(f: F[A] => F[A]): ProdF[F, A, B, Prim, SumTermId, ProductTermId] =
    ProdF(f(left), right)
  override def withRight(f: F[B] => F[B]): ProdF[F, A, B, Prim, SumTermId, ProductTermId] =
    ProdF(left, f(right))

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

  override type L = A
  override type R = A

  override def withLeft(f: F[A] => F[A]): BranchF[F, A, Prim, SumTermId, ProductTermId] =
    BranchF(id, f(schema))
  override def withRight(f: F[A] => F[A]): BranchF[F, A, Prim, SumTermId, ProductTermId] =
    BranchF(id, f(schema))

  override def inLeft: Option[F[L]]  = Some(schema)
  override def inRight: Option[F[R]] = Some(schema)

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

  override type L = AE
  override type R = AE

  override def withLeft(f: F[AE] => F[AE]): UnionF[F, A, AE, Prim, SumTermId, ProductTermId] =
    new UnionF[F, A, AE, Prim, SumTermId, ProductTermId](f(choices), iso) {}
  override def withRight(f: F[AE] => F[AE]): UnionF[F, A, AE, Prim, SumTermId, ProductTermId] =
    new UnionF[F, A, AE, Prim, SumTermId, ProductTermId](f(choices), iso) {}

  override def inLeft: Option[F[L]]  = Some(choices)
  override def inRight: Option[F[R]] = Some(choices)

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

  override type L = A
  override type R = A

  override def withLeft(f: F[A] => F[A]): FieldF[F, A, Prim, SumTermId, ProductTermId] =
    FieldF(id, f(schema))
  override def withRight(f: F[A] => F[A]): FieldF[F, A, Prim, SumTermId, ProductTermId] =
    FieldF(id, f(schema))

  override def inLeft: Option[F[L]]  = Some(schema)
  override def inRight: Option[F[R]] = Some(schema)

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    FieldF(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 * This class cannot be constructed directly, you must use the `SchemaModule#record` method.
 */
sealed abstract case class RecordF[F[_], A, AP, Prim[_], SumTermId, ProductTermId](
  fields: F[AP],
  iso: Iso[AP, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  override type L = AP
  override type R = AP

  override def withLeft(f: F[AP] => F[AP]): RecordF[F, A, AP, Prim, SumTermId, ProductTermId] =
    new RecordF[F, A, AP, Prim, SumTermId, ProductTermId](f(fields), iso) {}
  override def withRight(f: F[AP] => F[AP]): RecordF[F, A, AP, Prim, SumTermId, ProductTermId] =
    new RecordF[F, A, AP, Prim, SumTermId, ProductTermId](f(fields), iso) {}

  override def inLeft: Option[F[L]]  = Some(fields)
  override def inRight: Option[F[R]] = Some(fields)

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    new RecordF[G, A, AP, Prim, SumTermId, ProductTermId](nt(fields), iso) {}
}

/**
 * A sequence
 */
final case class SeqF[F[_], A, Prim[_], SumTermId, ProductTermId](element: F[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, List[A]] {

  override type L = A
  override type R = A

  override def withLeft(f: F[A] => F[A]): SeqF[F, A, Prim, SumTermId, ProductTermId] =
    SeqF(f(element))
  override def withRight(f: F[A] => F[A]): SeqF[F, A, Prim, SumTermId, ProductTermId] =
    SeqF(f(element))

  override def inLeft: Option[F[L]]  = Some(element)
  override def inRight: Option[F[R]] = Some(element)

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

  override type L = A0
  override type R = A0

  override def withLeft(f: F[A0] => F[A0]): IsoSchemaF[F, A0, A, Prim, SumTermId, ProductTermId] =
    IsoSchemaF(f(base), iso)
  override def withRight(f: F[A0] => F[A0]): IsoSchemaF[F, A0, A, Prim, SumTermId, ProductTermId] =
    IsoSchemaF(f(base), iso)

  override def inLeft: Option[F[L]]  = Some(base)
  override def inRight: Option[F[R]] = Some(base)

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    IsoSchemaF(nt(base), iso)
}

final case class SelfReference[F[_], H[_], A, Prim[_], SumTermId, ProductTermId](
  private val ref: () => F[A],
  private val nattrans: F ~> H
) extends SchemaF[Prim, SumTermId, ProductTermId, H, A] {

  override type L = A
  override type R = A

  override def withLeft(f: H[A] => H[A]): SelfReference[F, H, A, Prim, SumTermId, ProductTermId] =
    this
  override def withRight(f: H[A] => H[A]): SelfReference[F, H, A, Prim, SumTermId, ProductTermId] =
    this

  override def inLeft: Option[H[L]]  = Some(unroll)
  override def inRight: Option[H[R]] = Some(unroll)

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

  sealed private[schema] trait LabelledSum_[A, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchema[Prim, SumTermId, ProductTermId, A]

    def :+: [B](
      l: LabelledSum_[B, Prim, SumTermId, ProductTermId]
    ): LabelledSum_[B \/ A, Prim, SumTermId, ProductTermId] = LabelledSum2(l, this)
  }

  final private[schema] case class LabelledSum1[A, Prim[_], SumTermId, ProductTermId](
    id: SumTermId,
    schema: FSchema[Prim, SumTermId, ProductTermId, A]
  ) extends LabelledSum_[A, Prim, SumTermId, ProductTermId] {
    def toSchema = Fix(BranchF(id, schema))

  }

  final private[schema] case class LabelledSum2[A, B, Prim[_], SumTermId, ProductTermId](
    l: LabelledSum_[A, Prim, SumTermId, ProductTermId],
    r: LabelledSum_[B, Prim, SumTermId, ProductTermId]
  ) extends LabelledSum_[A \/ B, Prim, SumTermId, ProductTermId] {
    def toSchema = Fix(new SumF(l.toSchema, r.toSchema))

  }

  sealed private[schema] trait LabelledProduct_[A, Prim[_], SumTermId, ProductTermId] {
    def toSchema: FSchema[Prim, SumTermId, ProductTermId, A]

    def :*: [B](
      l: LabelledProduct_[B, Prim, SumTermId, ProductTermId]
    ): LabelledProduct_[(B, A), Prim, SumTermId, ProductTermId] = LabelledProduct2(l, this)
  }

  final private[schema] case class LabelledProduct1[A, Prim[_], SumTermId, ProductTermId](
    id: ProductTermId,
    schema: FSchema[Prim, SumTermId, ProductTermId, A]
  ) extends LabelledProduct_[A, Prim, SumTermId, ProductTermId] {
    def toSchema = Fix(FieldF(id, schema))

  }

  final private[schema] case class LabelledProduct2[A, B, Prim[_], SumTermId, ProductTermId](
    l: LabelledProduct_[A, Prim, SumTermId, ProductTermId],
    r: LabelledProduct_[B, Prim, SumTermId, ProductTermId]
  ) extends LabelledProduct_[(A, B), Prim, SumTermId, ProductTermId] {
    def toSchema = Fix(new ProdF(l.toSchema, r.toSchema))

  }
}

trait SchemaModule[R <: Realisation] {

  val R: R

  import SchemaF._

  type RInterpreter[F[_]] = Interpreter[Schema, F]

  type RSchema[F[_], A] = SchemaF[R.Prim, R.SumTermId, R.ProductTermId, F, A]

  type Schema[A] = FSchema[R.Prim, R.SumTermId, R.ProductTermId, A]

  type LabelledSum[A] = LabelledSum_[A, R.Prim, R.SumTermId, R.ProductTermId]

  type LabelledProduct[A] = LabelledProduct_[A, R.Prim, R.SumTermId, R.ProductTermId]

  type ROne[F[_]]            = One[F, R.Prim, R.SumTermId, R.ProductTermId]
  type Sum[F[_], A, B]       = SumF[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Prod[F[_], A, B]      = ProdF[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Branch[F[_], A]       = BranchF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Union[F[_], AE, A]    = UnionF[F, A, AE, R.Prim, R.SumTermId, R.ProductTermId]
  type Field[F[_], A]        = FieldF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Record[F[_], An, A]   = RecordF[F, A, An, R.Prim, R.SumTermId, R.ProductTermId]
  type Sequence[F[_], A]     = SeqF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type IsoSchema[F[_], A, B] = IsoSchemaF[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]

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

  implicit final class SchemaSyntax[A](schema: Schema[A]) {

    def :*: [B](left: Schema[B]): Schema[(B, A)] = Fix(new ProdF(left, schema))

    def :+: [B](left: Schema[B]): Schema[B \/ A] = Fix(new SumF(left, schema))

    def -*>: (id: R.ProductTermId): LabelledProduct[A] = LabelledProduct1(id, schema)

    def -+>: (id: R.SumTermId): LabelledSum[A] = LabelledSum1(id, schema)

    def to[F[_]](implicit interpreter: RInterpreter[F]): F[A] = interpreter.interpret(schema)

    def imap[B](_iso: Iso[A, B]): Schema[B] = schema.unFix match {
      case IsoSchemaF(base, iso) => Fix(IsoSchemaF(base, iso.composeIso(_iso)))
      case _                     => Fix(IsoSchemaF(schema, _iso))
    }

  }

  final def unit: Schema[Unit] =
    Fix(
      One[FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?], R.Prim, R.SumTermId, R.ProductTermId]()
    )

  final def prim[A](prim: R.Prim[A]): Schema[A] =
    Fix(PrimSchemaF(prim))

  final def union[A, AE](choices: LabelledSum[AE], iso: Iso[AE, A]): Schema[A] =
    Fix(
      new UnionF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        AE,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](choices.toSchema, iso) {}
    )

  final def optional[A](aSchema: Schema[A]): Schema[Option[A]] =
    iso(
      Fix(
        new SumF[
          FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
          A,
          Unit,
          R.Prim,
          R.SumTermId,
          R.ProductTermId
        ](aSchema, unit)
      ),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[A, An](terms: LabelledProduct[An], isoA: Iso[An, A]): Schema[A] =
    Fix(
      new RecordF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        An,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](terms.toSchema, isoA) {}
    )

  final def seq[A](element: Schema[A]): Schema[List[A]] =
    Fix(
      SeqF[
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        R.Prim,
        R.SumTermId,
        R.ProductTermId
      ](element)
    )

  final def iso[A0, A](base: Schema[A0], iso: Iso[A0, A]): Schema[A] =
    Fix(IsoSchemaF(base, iso))

  final def self[A](root: => Schema[A]): Schema[A] =
    Fix(SelfReference(() => root, λ[Schema ~> Schema](a => a)))
}
