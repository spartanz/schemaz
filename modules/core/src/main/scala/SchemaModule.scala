package scalaz

package schema

import monocle.Iso

final case class Fix[F[_[_], _], A](unFix: F[Fix[F, ?], A])

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

sealed trait Schema[Prim[_], SumTermId, ProductTermId, F[_], A] { //self =>
  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A]
  //    def imap[B](iso: Iso[A, B]): Schema.FSchema[B] = Schema.IsoSchema(self, iso)
}

////////////////////
// The Schema ADT
////////////////////

// "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

final case class One[Prim[_], SumTermId, ProductTermId, F[_]]()
    extends Schema[Prim, SumTermId, ProductTermId, F, Unit] {
  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, Unit] = One()
}

/**
 * The sum of two schemas, yielding the schema for `A \/ B`
 */
final case class :+:[Prim[_], SumTermId, ProductTermId, F[_], A, B](left: F[A], right: F[B])
    extends Schema[Prim, SumTermId, ProductTermId, F, A \/ B] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A \/ B] =
    :+:(nt(left), nt(right))
  override def toString: String = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class :*:[F[_], A, B, Prim[_], SumTermId, ProductTermId](left: F[A], right: F[B])
    extends Schema[Prim, SumTermId, ProductTermId, F, (A, B)] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, (A, B)] =
    :*:(nt(left), nt(right))
  override def toString: String = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchema[F[_], A, Prim[_], SumTermId, ProductTermId](prim: Prim[A])
    extends Schema[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] =
    PrimSchema[G, A, Prim, SumTermId, ProductTermId](prim)
}

/**
 * A named branch of an union
 */
final case class SumTerm[F[_], A, Prim[_], SumTermId, ProductTermId](id: SumTermId, schema: F[A])
    extends Schema[Prim, SumTermId, ProductTermId, F, A] {
  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] = SumTerm(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 */
final case class Union[Prim[_], SumTermId, ProductTermId, F[_], A, AE] private (
  choices: F[AE],
  iso: Iso[AE, A]
) extends Schema[Prim, SumTermId, ProductTermId, F, A] {
  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] = Union(nt(choices), iso)
}

/**
 * A named field of a record
 */
final case class ProductTerm[F[_], A, Prim[_], SumTermId, ProductTermId](
  id: ProductTermId,
  schema: F[A]
) extends Schema[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] =
    ProductTerm(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 */
final case class RecordSchema[Prim[_], SumTermId, ProductTermId, F[_], A, AP] private (
  fields: F[AP],
  iso: Iso[AP, A]
) extends Schema[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] =
    RecordSchema(nt(fields), iso)
}

/**
 * A sequence
 */
final case class SeqSchema[F[_], A, Prim[_], SumTermId, ProductTermId](element: F[A])
    extends Schema[Prim, SumTermId, ProductTermId, F, List[A]] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, List[A]] =
    SeqSchema(nt(element))
}

/**
 * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
 * between AO and A, then a schema of A0 can be used to represent values of A.
 */
final case class IsoSchema[Prim[_], SumTermId, ProductTermId, F[_], A0, A](
  base: F[A0],
  iso: Iso[A0, A]
) extends Schema[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[Prim, SumTermId, ProductTermId, G, A] =
    IsoSchema(nt(base), iso)
}

object Schema {

  // Writing final here triggers a warning, using sealed instead achieves almost the same effect
  // without warning. See https://issues.scala-lang.org/browse/SI-4440

  type FSchema[Prim[_], SumTermId, ProductTermId, A] =
    Fix[Schema[Prim, SumTermId, ProductTermId, ?[_], ?], A]

  // Schema syntax

  implicit final class SchemaSyntax[A, Prim[_], SumTermId, ProductTermId](
    schema: FSchema[Prim, SumTermId, ProductTermId, A]
  ) {

    def :*: [B](
      left: FSchema[Prim, SumTermId, ProductTermId, B]
    ): FSchema[Prim, SumTermId, ProductTermId, (B, A)] = Fix(new :*:(left, schema))

    def :+: [B](
      left: FSchema[Prim, SumTermId, ProductTermId, B]
    ): FSchema[Prim, SumTermId, ProductTermId, B \/ A] = Fix(new :+:(left, schema))

    def -*>: (id: ProductTermId): FSchema[Prim, SumTermId, ProductTermId, A] =
      Fix(ProductTerm(id, schema))
    def -+>: (id: SumTermId): FSchema[Prim, SumTermId, ProductTermId, A] = Fix(SumTerm(id, schema))

    def to[F[_]](
      implicit algebra: HAlgebra[Schema[Prim, SumTermId, ProductTermId, ?[_], ?], F]
    ): F[A] = cataNT(algebra)(schema)

    def imap[B](_iso: Iso[A, B]): FSchema[Prim, SumTermId, ProductTermId, B] = schema.unFix match {
      case IsoSchema(base, iso) => Fix(IsoSchema(base, iso.composeIso(_iso)))
      case _                    => Fix(IsoSchema(schema, _iso))
    }

  }

  /////////////////////////
  // Utility typeclasses
  /////////////////////////
  /*
   /**
   * Witnesses the fact that `T` is a product whose all members are `ProductTerm`s
   */
   trait LabelledProduct[T]

   implicit def labelledProduct[A, B, R[_] <: Schema[_]](
   implicit @deprecated("don't warn", "") proof: LabelledProduct[R[B]]
   ): LabelledProduct[:*:[A, ProductTerm, B, R]] =
   new LabelledProduct[:*:[A, ProductTerm, B, R]] {}

   implicit def singleLabelledProduct[A]: LabelledProduct[ProductTerm[A]] =
   new LabelledProduct[ProductTerm[A]] {}

   /**
   * Witnesses the fact that `T` is a sum whose all members are `SumTerm`s
   */
   trait LabelledSum[T]

   implicit def labelledSum[A, B, R[X] <: Schema[X]](
   implicit @deprecated("don't warn", "") proof: LabelledSum[R[B]]
   ): LabelledSum[:+:[A, SumTerm, B, R]] =
   new LabelledSum[:+:[A, SumTerm, B, R]] {}

   implicit def singleLabelledSum[A]: LabelledSum[SumTerm[A]] =
   new LabelledSum[SumTerm[A]] {}
   */
  ///////////////////////
  // Schema operations
  ///////////////////////

  type HAlgebra[F[_[_], _], G[_]] = F[G, ?] ~> G

  def cataNT[Prim[_], SumTermId, ProductTermId, F[_]](
    alg: HAlgebra[Schema[Prim, SumTermId, ProductTermId, ?[_], ?], F]
  ): (FSchema[Prim, SumTermId, ProductTermId, ?] ~> F) =
    new (FSchema[Prim, SumTermId, ProductTermId, ?] ~> F) { self =>

      def apply[A](f: FSchema[Prim, SumTermId, ProductTermId, A]): F[A] =
        alg.apply[A](f.unFix.hmap[F](self))
    }

}

trait SchemaModule[R <: Realisation] {

  val R: R

  import Schema._

  ////////////////
  // Public API
  ////////////////

  final def unit: FSchema[R.Prim, R.SumTermId, R.ProductTermId, Unit] =
    Fix(
      One[R.Prim, R.SumTermId, R.ProductTermId, FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?]]()
    )

  final def prim[A](prim: R.Prim[A]): FSchema[R.Prim, R.SumTermId, R.ProductTermId, A] =
    Fix(PrimSchema(prim))

  final def union[A, AE](
    choices: FSchema[R.Prim, R.SumTermId, R.ProductTermId, AE],
    iso: Iso[AE, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, A] =
    Fix(Union(choices, iso))

  final def optional[A](
    aSchema: FSchema[R.Prim, R.SumTermId, R.ProductTermId, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, Option[A]] =
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

  final def record[A, An](
    terms: FSchema[R.Prim, R.SumTermId, R.ProductTermId, An],
    isoA: Iso[An, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, A] =
    Fix(RecordSchema(terms, isoA))

  final def seq[A](
    element: FSchema[R.Prim, R.SumTermId, R.ProductTermId, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, List[A]] =
    Fix(SeqSchema(element))

  final def iso[A0, A](
    base: FSchema[R.Prim, R.SumTermId, R.ProductTermId, A0],
    iso: Iso[A0, A]
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, A] =
    Fix(IsoSchema(base, iso))

}
