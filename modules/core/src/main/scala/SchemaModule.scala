package scalaz

package schema

import recursion._

import monocle.Iso

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

sealed trait SchemaF[Prim[_], SumTermId, ProductTermId, F[_], A] { //self =>
  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A]
  //    def imap[B](iso: Iso[A, B]): Schema.FSchema[B] = Schema.IsoSchema(self, iso)
}

////////////////////
// The Schema ADT
////////////////////

// "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

final case class One[Prim[_], SumTermId, ProductTermId, F[_]]()
    extends SchemaF[Prim, SumTermId, ProductTermId, F, Unit] {
  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, Unit] = One()
}

/**
 * The sum of two schemas, yielding the schema for `A \/ B`
 */
final case class :+:[Prim[_], SumTermId, ProductTermId, F[_], A, B](left: F[A], right: F[B])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, A \/ B] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A \/ B] =
    :+:(nt(left), nt(right))
  override def toString: String = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class :*:[F[_], A, B, Prim[_], SumTermId, ProductTermId](left: F[A], right: F[B])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, (A, B)] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, (A, B)] =
    :*:(nt(left), nt(right))
  override def toString: String = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchema[F[_], A, Prim[_], SumTermId, ProductTermId](prim: Prim[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    PrimSchema[G, A, Prim, SumTermId, ProductTermId](prim)
}

/**
 * A named branch of an union
 */
final case class SumTerm[F[_], A, Prim[_], SumTermId, ProductTermId](id: SumTermId, schema: F[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    SumTerm(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 * This class cannot be constructed directly, you must use the `SchemaModule#union` method.
 */
sealed abstract case class Union[Prim[_], SumTermId, ProductTermId, F[_], A, AE](
  choices: F[AE],
  iso: Iso[AE, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    new Union[Prim, SumTermId, ProductTermId, G, A, AE](nt(choices), iso) {}
}

/**
 * A named field of a record
 */
final case class ProductTerm[F[_], A, Prim[_], SumTermId, ProductTermId](
  id: ProductTermId,
  schema: F[A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    ProductTerm(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 * This class cannot be constructed directly, you must use the `SchemaModule#record` method.
 */
sealed abstract case class Record[Prim[_], SumTermId, ProductTermId, F[_], A, AP](
  fields: F[AP],
  iso: Iso[AP, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    new Record[Prim, SumTermId, ProductTermId, G, A, AP](nt(fields), iso) {}
}

/**
 * A sequence
 */
final case class SeqSchema[F[_], A, Prim[_], SumTermId, ProductTermId](element: F[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, List[A]] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, List[A]] =
    SeqSchema(nt(element))
}

/**
 * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
 * between AO and A, then a schema of A0 can be used to represent values of A.
 */
final case class IsoSchema[Prim[_], SumTermId, ProductTermId, F[_], A0, A](
  base: F[A0],
  iso: Iso[A0, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    IsoSchema(nt(base), iso)
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

  sealed private[schema] trait LabelledSum_[Prim[_], SumTermId, ProductTermId, A] {
    def toSchema: FSchema[Prim, SumTermId, ProductTermId, A]

    def :+: [B](
      l: LabelledSum_[Prim, SumTermId, ProductTermId, B]
    ): LabelledSum_[Prim, SumTermId, ProductTermId, B \/ A] = LabelledSum2(l, this)
  }

  final private[schema] case class LabelledSum1[Prim[_], SumTermId, ProductTermId, A](
    id: SumTermId,
    schema: FSchema[Prim, SumTermId, ProductTermId, A]
  ) extends LabelledSum_[Prim, SumTermId, ProductTermId, A] {
    def toSchema = Fix(SumTerm(id, schema))

  }

  final private[schema] case class LabelledSum2[Prim[_], SumTermId, ProductTermId, A, B](
    l: LabelledSum_[Prim, SumTermId, ProductTermId, A],
    r: LabelledSum_[Prim, SumTermId, ProductTermId, B]
  ) extends LabelledSum_[Prim, SumTermId, ProductTermId, A \/ B] {
    def toSchema = Fix(new :+:(l.toSchema, r.toSchema))

  }

  sealed private[schema] trait LabelledProduct_[Prim[_], SumTermId, ProductTermId, A] {
    def toSchema: FSchema[Prim, SumTermId, ProductTermId, A]

    def :*: [B](
      l: LabelledProduct_[Prim, SumTermId, ProductTermId, B]
    ): LabelledProduct_[Prim, SumTermId, ProductTermId, (B, A)] = LabelledProduct2(l, this)
  }

  final private[schema] case class LabelledProduct1[Prim[_], SumTermId, ProductTermId, A](
    id: ProductTermId,
    schema: FSchema[Prim, SumTermId, ProductTermId, A]
  ) extends LabelledProduct_[Prim, SumTermId, ProductTermId, A] {
    def toSchema = Fix(ProductTerm(id, schema))

  }

  final private[schema] case class LabelledProduct2[Prim[_], SumTermId, ProductTermId, A, B](
    l: LabelledProduct_[Prim, SumTermId, ProductTermId, A],
    r: LabelledProduct_[Prim, SumTermId, ProductTermId, B]
  ) extends LabelledProduct_[Prim, SumTermId, ProductTermId, (A, B)] {
    def toSchema = Fix(new :*:(l.toSchema, r.toSchema))

  }
}

trait SchemaModule[R <: Realisation] {

  val R: R

  import SchemaF._

  type RInterpreter[F[_]] = Interpreter[Schema, F]

  type RSchema[F[_], A] = SchemaF[R.Prim, R.SumTermId, R.ProductTermId, F, A]

  type Schema[A] = FSchema[R.Prim, R.SumTermId, R.ProductTermId, A]

  type LabelledSum[A] = LabelledSum_[R.Prim, R.SumTermId, R.ProductTermId, A]

  type LabelledProduct[A] = LabelledProduct_[R.Prim, R.SumTermId, R.ProductTermId, A]

  type ROne[F[_]]            = One[R.Prim, R.SumTermId, R.ProductTermId, F]
  type RSum[F[_], A, B]      = :+:[R.Prim, R.SumTermId, R.ProductTermId, F, A, B]
  type RProduct[F[_], A, B]  = :*:[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type RSumTerm[F[_], A]     = SumTerm[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type RUnion[F[_], AE, A]   = Union[R.Prim, R.SumTermId, R.ProductTermId, F, A, AE]
  type RProductTerm[F[_], A] = ProductTerm[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type RRecord[F[_], An, A]  = Record[R.Prim, R.SumTermId, R.ProductTermId, F, A, An]
  type RSeq[F[_], A]         = SeqSchema[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type RIso[F[_], A, B]      = IsoSchema[R.Prim, R.SumTermId, R.ProductTermId, F, A, B]

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

    def :*: [B](left: Schema[B]): Schema[(B, A)] = Fix(new :*:(left, schema))

    def :+: [B](left: Schema[B]): Schema[B \/ A] = Fix(new :+:(left, schema))

    def -*>: (id: R.ProductTermId): LabelledProduct[A] = LabelledProduct1(id, schema)

    def -+>: (id: R.SumTermId): LabelledSum[A] = LabelledSum1(id, schema)

    def to[F[_]](implicit interpreter: RInterpreter[F]): F[A] = interpreter.interpret(schema)

    def imap[B](_iso: Iso[A, B]): Schema[B] = schema.unFix match {
      case IsoSchema(base, iso) => Fix(IsoSchema(base, iso.composeIso(_iso)))
      case _                    => Fix(IsoSchema(schema, _iso))
    }

  }

  final def unit: Schema[Unit] =
    Fix(
      One[R.Prim, R.SumTermId, R.ProductTermId, FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?]]()
    )

  final def prim[A](prim: R.Prim[A]): Schema[A] =
    Fix(PrimSchema(prim))

  final def union[A, AE](choices: LabelledSum[AE], iso: Iso[AE, A]): Schema[A] =
    Fix(
      new Union[
        R.Prim,
        R.SumTermId,
        R.ProductTermId,
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        AE
      ](choices.toSchema, iso) {}
    )

  final def optional[A](aSchema: Schema[A]): Schema[Option[A]] =
    iso(
      Fix(
        new :+:[
          R.Prim,
          R.SumTermId,
          R.ProductTermId,
          FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
          A,
          Unit
        ](aSchema, unit)
      ),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[A, An](terms: LabelledProduct[An], isoA: Iso[An, A]): Schema[A] =
    Fix(
      new Record[
        R.Prim,
        R.SumTermId,
        R.ProductTermId,
        FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?],
        A,
        An
      ](terms.toSchema, isoA) {}
    )

  final def seq[A](element: Schema[A]): Schema[List[A]] =
    Fix(SeqSchema(element))

  final def iso[A0, A](base: Schema[A0], iso: Iso[A0, A]): Schema[A] =
    Fix(IsoSchema(base, iso))

}
