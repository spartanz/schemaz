package scalaz

package schema

import std.list.listInstance
import monocle.Iso

trait SchemaModule {
  type Prim[A]
  type SumTermId
  type ProductTermId

  def prim[A](prim: Prim[A]): Schema[A] = Schema.PrimSchema(prim)

  def seq[A](element: Schema[A]): Schema[List[A]] = Schema.SeqSchema(element)

  def iso[A0, A](base: Schema[A0], iso: Iso[A0, A]): Schema[A] = Schema.IsoSchema(base, iso)

  sealed trait Schema[A] { self =>
    def imap[B](iso: Iso[A, B]): Schema[B] = Schema.IsoSchema(self, iso)
  }

  def union[A, AE, R <: Schema[AE]](choices: R, iso: Iso[AE, A])(
    implicit @deprecated("don't warn", "") proof: Schema.LabelledSum[R]
  ): Schema.Union[A, AE] = Schema.Union(choices, iso)

  def optional[A, L[_] <: Schema[_]](aSchema: L[A]): Schema[Option[A]] =
    iso(
      new Schema.:+:[A, L, Unit, Schema](aSchema, Schema.Zero),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  def record[A, An, R <: Schema[An]](terms: R, isoA: Iso[An, A])(
    implicit @deprecated("don't warn", "") proof: Schema.LabelledProduct[R]
  ): Schema[A] =
    Schema.RecordSchema[A, An](
      terms,
      isoA
    )

  object Schema {

    trait LabelledProduct[T]

    implicit def labelledProduct[A, B, R[_] <: Schema[_]](
      implicit @deprecated("don't warn", "") proof: LabelledProduct[R[B]]
    ): LabelledProduct[:*:[A, ProductTerm, B, R]] =
      new LabelledProduct[:*:[A, ProductTerm, B, R]] {}

    implicit def singleLabelledProduct[A]: LabelledProduct[ProductTerm[A]] =
      new LabelledProduct[ProductTerm[A]] {}

    trait LabelledSum[T]

    implicit def labelledSum[A, B, R[_] <: Schema[_]](
      implicit @deprecated("don't warn", "") proof: LabelledSum[R[B]]
    ): LabelledSum[:+:[A, SumTerm, B, R]] =
      new LabelledSum[:+:[A, SumTerm, B, R]] {}

    implicit def singleLabelledSum[A]: LabelledSum[SumTerm[A]] =
      new LabelledSum[SumTerm[A]] {}

    sealed case class SumTerm[A](id: SumTermId, schema: Schema[A]) extends Schema[A]

    sealed case class ProductTerm[A](id: ProductTermId, schema: Schema[A]) extends Schema[A]

    sealed class :+:[A, L[_] <: Schema[_], B, R[_] <: Schema[_]](l: => L[A], r: => R[B])
        extends Schema[A \/ B] {
      lazy val left: L[A]  = l
      lazy val right: R[B] = r

      override def toString: String = s"$left :+: $right"
    }

    object :*: {

      def unapply[A, L[_] <: Schema[_], B, R[_] <: Schema[_]](
        prod: :*:[A, L, B, R]
      ): Option[(L[A], R[B])] = Some((prod.left, prod.right))
    }

    case object Zero extends Schema[Unit]

    sealed class :*:[A, L[_] <: Schema[_], B, R[_] <: Schema[_]](
      l: => L[A],
      r: => R[B]
    ) extends Schema[(A, B)] {
      lazy val left: L[A]  = l
      lazy val right: R[B] = r

      override def toString: String = s"$left :*: $right"
    }

    object One extends Schema[Unit]

    // Writing final here triggers a warning, using sealed instead achieves almost the same effect
    // without warning. See https://issues.scala-lang.org/browse/SI-4440
    sealed case class PrimSchema[A](prim: Prim[A]) extends Schema[A]
    sealed case class Union[A, AE](
      choices: Schema[AE],
      iso: Iso[AE, A]
    ) extends Schema[A]
    sealed case class RecordSchema[A, AP] private (
      fields: Schema[AP],
      iso: Iso[AP, A]
    ) extends Schema[A]
    sealed case class SeqSchema[A](element: Schema[A]) extends Schema[List[A]]
    sealed case class IsoSchema[A0, A](base: Schema[A0], iso: Iso[A0, A]) extends Schema[A] {
      override def imap[B](_iso: Iso[A, B]): Schema[B] =
        IsoSchema(base, iso.composeIso(_iso))
    }

    implicit class SchemaExtensions[A, R[_] <: Schema[_]](schema: R[A]) {
      def :*: [B, L[_] <: Schema[_]](left: L[B]): :*:[B, L, A, R] = new :*:(left, schema)
      def :+: [B, L[_] <: Schema[_]](left: L[B]): :+:[B, L, A, R] = new :+:(left, schema)
      def -*>: (id: ProductTermId): ProductTerm[A]                = ProductTerm(id, schema.asInstanceOf[Schema[A]])
      def -+>: (id: SumTermId): SumTerm[A]                        = SumTerm(id, schema.asInstanceOf[Schema[A]])
    }

    trait Representation[F[_]] {
      def prims: Prim ~> F
      def handleRecord: F ~> F
      def labelField(label: ProductTermId): F ~> F
      def handleUnion: F ~> F
      def labelBranch(label: SumTermId): F ~> F
      def handleList[A]: F[A] => F[List[A]]
      def zero: F[Unit]
    }

    def covariantFold[F[_]](representation: Representation[F])(implicit F: Alt[F]): Schema ~> F =
      new (Schema ~> F) {

        def apply[A](schema: Schema[A]): F[A] = schema match {
          case t: PrimSchema[a] => representation.prims(t.prim)
          case p: :*:[a, l, b, r] =>
            F.tuple2[a, b](
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

          case Zero => representation.zero
          case _    => ???
        }
      }

    def contravariantFold[F[_]](
      representation: Representation[F]
    )(implicit F: Decidable[F]): Schema ~> F =
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
          case Zero => representation.zero

        }
      }
  }

}
