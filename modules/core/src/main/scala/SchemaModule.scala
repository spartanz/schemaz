package scalaz

package schema

import monocle.Iso

final case class Fix[F[_[_], _], A](unFix: F[Fix[F, ?], A])

trait SchemaModule {
  type Prim[A]
  type SumTermId
  type ProductTermId

  sealed trait Schema[F[_], A] { //self =>

    def hmap[G[_]](nt: F ~> G): Schema[G, A]
//    def imap[B](iso: Iso[A, B]): Schema.FSchema[B] = Schema.IsoSchema(self, iso)
  }

  object Schema {

    // Writing final here triggers a warning, using sealed instead achieves almost the same effect
    // without warning. See https://issues.scala-lang.org/browse/SI-4440

    ////////////////////
    // The Schema ADT
    ////////////////////

    // "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

    sealed case class One[F[_]]() extends Schema[F, Unit] {
      def hmap[G[_]](nt: F ~> G): Schema[G, Unit] = One()
    }

    /**
     * The sum of two schemas, yielding the schema for `A \/ B`
     */
    sealed case class :+:[F[_], A, B](left: F[A], right: F[B]) extends Schema[F, A \/ B] {
      def hmap[G[_]](nt: F ~> G): Schema[G, A \/ B] = :+:(nt(left), nt(right))
      override def toString: String                 = s"$left :+: $right"
    }

    /**
     * The product of two schemas, yielding the schema for `(A, B)`
     */
    sealed case class :*:[F[_], A, B](left: F[A], right: F[B]) extends Schema[F, (A, B)] {
      def hmap[G[_]](nt: F ~> G): Schema[G, (A, B)] = :*:(nt(left), nt(right))
      override def toString: String                 = s"$left :*: $right"
    }

    // "Extra" nodes, making it more convenient to represent real-world types

    /**
     * The schema of a primitive type in the context of this `SchemaModule`
     */
    sealed case class PrimSchema[F[_], A](prim: Prim[A]) extends Schema[F, A] {
      def hmap[G[_]](nt: F ~> G): Schema[G, A] = PrimSchema[G, A](prim)
    }

    /**
     * A named branch of an union
     */
    sealed case class SumTerm[F[_], A](id: SumTermId, schema: F[A]) extends Schema[F, A] {
      def hmap[G[_]](nt: F ~> G): Schema[G, A] = SumTerm(id, nt(schema))
    }

    /**
     * An union, eg. a sum of named branches
     */
    sealed case class Union[F[_], A, AE] private (choices: F[AE], iso: Iso[AE, A])
        extends Schema[F, A] {
      def hmap[G[_]](nt: F ~> G): Schema[G, A] = Union(nt(choices), iso)
    }

    /**
     * A named field of a record
     */
    sealed case class ProductTerm[F[_], A](id: ProductTermId, schema: F[A]) extends Schema[F, A] {
      def hmap[G[_]](nt: F ~> G): Schema[G, A] = ProductTerm(id, nt(schema))
    }

    /**
     * A record, eg. a product of named fields
     */
    sealed case class RecordSchema[F[_], A, AP] private (fields: F[AP], iso: Iso[AP, A])
        extends Schema[F, A] {
      def hmap[G[_]](nt: F ~> G): Schema[G, A] = RecordSchema(nt(fields), iso)
    }

    /**
     * A sequence
     */
    sealed case class SeqSchema[F[_], A](element: F[A]) extends Schema[F, List[A]] {
      def hmap[G[_]](nt: F ~> G): Schema[G, List[A]] = SeqSchema(nt(element))
    }

    /**
     * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
     * between AO and A, then a schema of A0 can be used to represent values of A.
     */
    sealed case class IsoSchema[F[_], A0, A](base: F[A0], iso: Iso[A0, A]) extends Schema[F, A] {

      def hmap[G[_]](nt: F ~> G): Schema[G, A] = IsoSchema(nt(base), iso)
    }

    // Schema syntax

    implicit final class SchemaSyntax[A](schema: Fix[Schema, A]) {
      def :*: [B](left: Fix[Schema, B]): Fix[Schema, (B, A)]    = Fix(new :*:(left, schema))
      def :+: [B](left: Fix[Schema, B]): Fix[Schema, B \/ A]    = Fix(new :+:(left, schema))
      def -*>: (id: ProductTermId): Fix[Schema, A]              = Fix(ProductTerm(id, schema))
      def -+>: (id: SumTermId): Fix[Schema, A]                  = Fix(SumTerm(id, schema))
      def to[F[_]](implicit algebra: HAlgebra[Schema, F]): F[A] = cataNT(algebra)(schema)

      def imap[B](_iso: Iso[A, B]): Fix[Schema, B] = schema.unFix match {
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

    def cataNT[F[_]](alg: HAlgebra[Schema, F]): (FSchema ~> F) =
      new (FSchema ~> F) { self =>

        def apply[A](f: Fix[Schema, A]): F[A] =
          alg.apply[A](f.unFix.hmap[F](self))
      }

    type FSchema[A] = Fix[Schema, A]

  }

  ////////////////
  // Public API
  ////////////////

  final def unit: Schema.FSchema[Unit] = Fix(Schema.One[Schema.FSchema]())

  final def prim[A](prim: Prim[A]): Schema.FSchema[A] = Fix(Schema.PrimSchema(prim))

  final def union[A, AE](choices: Schema.FSchema[AE], iso: Iso[AE, A]) /*(
    implicit @deprecated("don't warn", "") proof: Schema.LabelledSum[R]
  )*/: Schema.FSchema[A] = Fix(Schema.Union(choices, iso))

  final def optional[A](aSchema: Schema.FSchema[A]): Schema.FSchema[Option[A]] =
    iso(
      Fix(new Schema.:+:[Schema.FSchema, A, Unit](aSchema, unit)),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[A, An](terms: Schema.FSchema[An], isoA: Iso[An, A]) /*(
    implicit @deprecated("don't warn", "") proof: Schema.LabelledProduct[R]
  )*/: Schema.FSchema[A] = Fix(Schema.RecordSchema(terms, isoA))

  final def seq[A](element: Schema.FSchema[A]): Schema.FSchema[List[A]] =
    Fix[Schema, List[A]](Schema.SeqSchema(element))

  final def iso[A0, A](base: Schema.FSchema[A0], iso: Iso[A0, A]): Schema.FSchema[A] =
    Fix(Schema.IsoSchema(base, iso))

}
