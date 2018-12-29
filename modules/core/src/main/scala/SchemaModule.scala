package scalaz

package schema

import monocle.Iso

final case class Fix[F[_[_], _], A](unFix: F[Fix[F, ?], A])

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

sealed trait Schema[R <: Realisation, F[_], A] { //self =>

  def hmap[G[_]](nt: F ~> G): Schema[R, G, A]
  //    def imap[B](iso: Iso[A, B]): Schema.FSchema[B] = Schema.IsoSchema(self, iso)
}

////////////////////
// The Schema ADT
////////////////////

// "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

final case class One[R <: Realisation, F[_]]() extends Schema[R, F, Unit] {
  def hmap[G[_]](nt: F ~> G): Schema[R, G, Unit] = One()
}

/**
 * The sum of two schemas, yielding the schema for `A \/ B`
 */
final case class :+:[R <: Realisation, F[_], A, B](left: F[A], right: F[B])
    extends Schema[R, F, A \/ B] {
  def hmap[G[_]](nt: F ~> G): Schema[R, G, A \/ B] = :+:(nt(left), nt(right))
  override def toString: String                    = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class :*:[F[_], A, B, R <: Realisation](left: F[A], right: F[B])
    extends Schema[R, F, (A, B)] {
  def hmap[G[_]](nt: F ~> G): Schema[R, G, (A, B)] = :*:(nt(left), nt(right))
  override def toString: String                    = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchema[F[_], A, R <: Realisation](prim: R#Prim[A]) extends Schema[R, F, A] {
  def hmap[G[_]](nt: F ~> G): Schema[R, G, A] = PrimSchema[G, A, R](prim)
}

/**
 * A named branch of an union
 */
final case class SumTerm[F[_], A, R <: Realisation](id: R#SumTermId, schema: F[A])
    extends Schema[R, F, A] {
  def hmap[G[_]](nt: F ~> G): Schema[R, G, A] = SumTerm(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 */
final case class Union[R <: Realisation, F[_], A, AE] private (choices: F[AE], iso: Iso[AE, A])
    extends Schema[R, F, A] {
  def hmap[G[_]](nt: F ~> G): Schema[R, G, A] = Union(nt(choices), iso)
}

/**
 * A named field of a record
 */
final case class ProductTerm[F[_], A, R <: Realisation](id: R#ProductTermId, schema: F[A])
    extends Schema[R, F, A] {
  def hmap[G[_]](nt: F ~> G): Schema[R, G, A] = ProductTerm(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 */
final case class RecordSchema[R <: Realisation, F[_], A, AP] private (
  fields: F[AP],
  iso: Iso[AP, A]
) extends Schema[R, F, A] {
  def hmap[G[_]](nt: F ~> G): Schema[R, G, A] = RecordSchema(nt(fields), iso)
}

/**
 * A sequence
 */
final case class SeqSchema[F[_], A, R <: Realisation](element: F[A]) extends Schema[R, F, List[A]] {
  def hmap[G[_]](nt: F ~> G): Schema[R, G, List[A]] = SeqSchema(nt(element))
}

/**
 * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
 * between AO and A, then a schema of A0 can be used to represent values of A.
 */
final case class IsoSchema[R <: Realisation, F[_], A0, A](base: F[A0], iso: Iso[A0, A])
    extends Schema[R, F, A] {

  def hmap[G[_]](nt: F ~> G): Schema[R, G, A] = IsoSchema(nt(base), iso)
}

object Schema {

  // Writing final here triggers a warning, using sealed instead achieves almost the same effect
  // without warning. See https://issues.scala-lang.org/browse/SI-4440

  type FSchema[R <: Realisation, A] = Fix[Schema[R, ?[_], ?], A]

  // Schema syntax

  implicit final class SchemaSyntax[A, R <: Realisation](schema: FSchema[R, A]) {
    def :*: [B](left: FSchema[R, B]): FSchema[R, (B, A)]                  = Fix(new :*:(left, schema))
    def :+: [B](left: FSchema[R, B]): FSchema[R, B \/ A]                  = Fix(new :+:(left, schema))
    def -*>: (id: R#ProductTermId): FSchema[R, A]                         = Fix(ProductTerm(id, schema))
    def -+>: (id: R#SumTermId): FSchema[R, A]                             = Fix(SumTerm(id, schema))
    def to[F[_]](implicit algebra: HAlgebra[Schema[R, ?[_], ?], F]): F[A] = cataNT(algebra)(schema)

    def imap[B](_iso: Iso[A, B]): FSchema[R, B] = schema.unFix match {
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

  def cataNT[R <: Realisation, F[_]](alg: HAlgebra[Schema[R, ?[_], ?], F]): (FSchema[R, ?] ~> F) =
    new (FSchema[R, ?] ~> F) { self =>

      def apply[A](f: FSchema[R, A]): F[A] =
        alg.apply[A](f.unFix.hmap[F](self))
    }

}

trait SchemaModule[R <: Realisation] {

  import Schema._

  ////////////////
  // Public API
  ////////////////

  final def unit: FSchema[R, Unit] = Fix(One[R, FSchema[R, ?]]())

  final def prim[A](prim: R#Prim[A]): FSchema[R, A] = Fix(PrimSchema(prim))

  final def union[A, AE](
    choices: FSchema[R, AE],
    iso: Iso[AE, A]
  ): FSchema[R, A] =
    Fix(Union(choices, iso))

  final def optional[A](aSchema: FSchema[R, A]): FSchema[R, Option[A]] =
    iso(
      Fix(new :+:[R, FSchema[R, ?], A, Unit](aSchema, unit)),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[A, An](
    terms: FSchema[R, An],
    isoA: Iso[An, A]
  ): FSchema[R, A] =
    Fix(RecordSchema(terms, isoA))

  final def seq[A](element: FSchema[R, A]): FSchema[R, List[A]] =
    Fix(SeqSchema(element))

  final def iso[A0, A](base: FSchema[R, A0], iso: Iso[A0, A]): FSchema[R, A] =
    Fix(IsoSchema(base, iso))

}
