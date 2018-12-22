package scalaz

package schema

import monocle.Iso

trait SchemaModule {
  type Prim[A]
  type SumTermId
  type ProductTermId

  sealed trait Schema[A] { self =>
    def imap[B](iso: Iso[A, B]): Schema[B] = Schema.IsoSchema(self, iso)
  }

  object Schema {

    // Writing final here triggers a warning, using sealed instead achieves almost the same effect
    // without warning. See https://issues.scala-lang.org/browse/SI-4440

    ////////////////////
    // The Schema ADT
    ////////////////////

    // "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

    object One extends Schema[Unit]

    /**
     * The sum of two schemas, yielding the schema for `A \/ B`
     */
    sealed class :+:[A, L[X] <: Schema[X], B, R[Y] <: Schema[Y]](l: => L[A], r: => R[B])
        extends Schema[A \/ B] {
      lazy val left: L[A]  = l
      lazy val right: R[B] = r

      override def toString: String = s"$left :+: $right"
    }

    /**
     * The product of two schemas, yielding the schema for `(A, B)`
     */
    sealed class :*:[A, L[_] <: Schema[_], B, R[_] <: Schema[_]](l: => L[A], r: => R[B])
        extends Schema[(A, B)] {
      lazy val left: L[A]  = l
      lazy val right: R[B] = r

      override def toString: String = s"$left :*: $right"
    }

    // "Extra" nodes, making it more convenient to represent real-world types

    /**
     * The schema of a primitive type in the context of this `SchemaModule`
     */
    sealed case class PrimSchema[A](prim: Prim[A]) extends Schema[A]

    /**
     * A named branch of an union
     */
    sealed case class SumTerm[A](id: SumTermId, schema: Schema[A]) extends Schema[A]

    /**
     * An union, eg. a sum of named branches
     */
    sealed case class Union[A, AE] private (choices: Schema[AE], iso: Iso[AE, A]) extends Schema[A]

    /**
     * A named field of a record
     */
    sealed case class ProductTerm[A](id: ProductTermId, schema: Schema[A]) extends Schema[A]

    /**
     * A record, eg. a product of named fields
     */
    sealed case class RecordSchema[A, AP] private (fields: Schema[AP], iso: Iso[AP, A])
        extends Schema[A]

    /**
     * A sequence
     */
    sealed case class SeqSchema[A](element: Schema[A]) extends Schema[List[A]]

    /**
     * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
     * between AO and A, then a schema of A0 can be used to represent values of A.
     */
    sealed case class IsoSchema[A0, A](base: Schema[A0], iso: Iso[A0, A]) extends Schema[A] {
      override def imap[B](_iso: Iso[A, B]): Schema[B] =
        IsoSchema(base, iso.composeIso(_iso))
    }

    // Schema syntax

    implicit class SchemaSyntax[A, R[X] <: Schema[X]](schema: R[A]) {
      def :*: [B, L[Y] <: Schema[Y]](left: L[B]): :*:[B, L, A, R] = new :*:(left, schema)
      def :+: [B, L[Y] <: Schema[Y]](left: L[B]): :+:[B, L, A, R] = new :+:(left, schema)
      def -*>: (id: ProductTermId): ProductTerm[A]                = ProductTerm(id, schema)
      def -+>: (id: SumTermId): SumTerm[A]                        = SumTerm(id, schema)
      def to[F[_]](implicit F: FoldableTo[F]): F[A]               = F.fold.apply(schema)
    }

    /////////////////////////
    // Utility typeclasses
    /////////////////////////

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

    ///////////////////////
    // Schema operations
    ///////////////////////

    /**
     * Required operations to handle leaves and "special cases" (records, unions, sequences)
     * in the context of `F`.
     */
    trait Representation[F[_]] {

      /**
       * Way to handle this `SchemaModule`'s primitive types in the context of `F`.
       */
      def prims: Prim ~> F

      /**
       * Hook for special handling of records in the context of `F`.
       */
      def handleRecord: F ~> F = NaturalTransformation.refl[F]

      /**
       * Hook for handling labelling of record fields in the context of `F`.
       * This is encoded as a lambda so that we can provide a default implementation
       * that doesn't use the label without triggering an "unused parameter" warning.
       */
      def labelField: ProductTermId => F ~> F = l => NaturalTransformation.refl[F]

      /**
       * Hook for special handling of unions in the context of `F`.
       */
      def handleUnion: F ~> F = NaturalTransformation.refl[F]

      /**
       * Hook for handling labelling of union branches in the context of `F`.
       * This is encoded as a lambda so that we can provide a default implementation
       * that doesn't use the label without triggering an "unused parameter" warning.
       */
      def labelBranch: SumTermId => F ~> F = l => NaturalTransformation.refl[F]

      /**
       * Hook for special handling of sequences in the context of `F`.
       */
      def handleList[A]: F[A] => F[List[A]]

      /**
       * The representation of `Unit` in the context of `F`.
       */
      def unit: F[Unit]
    }

    /**
     * Witnesses the fact that `F` has enough capabilities to be deduced from a `Schema`
     */
    trait FoldableTo[F[_]] {
      def fold: Schema ~> F
    }

    implicit def covariantFoldableTo[F[_]](implicit F: Alt[F], representation: Representation[F]) =
      new FoldableTo[F] {

        def fold: Schema ~> F =
          new (Schema ~> F) {

            def apply[A](schema: Schema[A]): F[A] = schema match {
              case t: PrimSchema[a] => representation.prims(t.prim)
              case p: :*:[a, l, b, r] =>
                F.tuple2[a, b](
                  // Apparently, scalac needs a little help here
                  // from the signature of :*:, l[a] <: Schema[a]
                  // should be obvious
                  apply(p.left.asInstanceOf[Schema[a]]),
                  apply(p.right.asInstanceOf[Schema[b]])
                )
              case s: :+:[a, l, b, r] =>
                F.either2(
                  apply(s.left.asInstanceOf[Schema[a]]),
                  apply(s.right.asInstanceOf[Schema[b]])
                )
              case i: IsoSchema[a0, a] =>
                F.map(apply(i.base))(i.iso.get)
              case r: RecordSchema[a, an] =>
                representation.handleRecord(F.map(apply(r.fields))(r.iso.get))
              case s: SeqSchema[a] =>
                representation.handleList(apply(s.element))
              case ProductTerm(id, base) => representation.labelField(id)(apply(base))
              case u: Union[a, ae] =>
                representation.handleUnion(F.map(apply(u.choices))(u.iso.get))
              case SumTerm(id, base) => representation.labelBranch(id)(apply(base))
              case One               => representation.unit
            }
          }
      }

    implicit def contravariantFoldableTo[F[_]](
      implicit F: Decidable[F],
      representation: Representation[F]
    ) = new FoldableTo[F] {

      def fold: Schema ~> F =
        new (Schema ~> F) {

          def apply[A](schema: Schema[A]): F[A] = schema match {
            case t: PrimSchema[a] => representation.prims(t.prim)
            case p: :*:[a, l, b, r] =>
              F.divide2(
                apply(p.left.asInstanceOf[Schema[a]]),
                apply(p.right.asInstanceOf[Schema[b]])
              )(identity)
            case s: :+:[a, l, b, r] =>
              F.choose2(
                apply(s.left.asInstanceOf[Schema[a]]),
                apply(s.right.asInstanceOf[Schema[b]])
              )(identity)
            case i: IsoSchema[a0, a] =>
              F.contramap(apply(i.base))(i.iso.apply)
            case r: RecordSchema[a, an] =>
              representation.handleRecord(F.contramap(apply(r.fields))(r.iso.apply))
            case ProductTerm(id, base) => representation.labelField(id)(apply(base))
            case u: Union[a, ae] =>
              representation.handleUnion(F.contramap(apply(u.choices))(u.iso.apply))
            case SumTerm(id, base) => representation.labelBranch(id)(apply(base))
            case s: SeqSchema[a] =>
              representation.handleList(apply(s.element))
            case One => representation.unit
          }
        }
    }
  }

  ////////////////
  // Public API
  ////////////////

  def prim[A](prim: Prim[A]): Schema[A] = Schema.PrimSchema(prim)

  def union[A, AE, R <: Schema[AE]](choices: R, iso: Iso[AE, A])(
    implicit @deprecated("don't warn", "") proof: Schema.LabelledSum[R]
  ): Schema[A] = Schema.Union[A, AE](choices, iso)

  def optional[A, L[X] <: Schema[X]](aSchema: L[A]): Schema[Option[A]] =
    iso(
      new Schema.:+:[A, L, Unit, Schema](aSchema, Schema.One),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  def record[A, An, R <: Schema[An]](terms: R, isoA: Iso[An, A])(
    implicit @deprecated("don't warn", "") proof: Schema.LabelledProduct[R]
  ): Schema[A] = Schema.RecordSchema[A, An](terms, isoA)

  def seq[A](element: Schema[A]): Schema[List[A]] = Schema.SeqSchema(element)

  def iso[A0, A](base: Schema[A0], iso: Iso[A0, A]): Schema[A] = Schema.IsoSchema(base, iso)

}
