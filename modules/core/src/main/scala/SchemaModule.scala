package scalaz

package schema

import monocle.Iso

trait SchemaModule {
  type Prim[A]
  type SumTermId
  type ProductTermId

  def prim[A](prim: Prim[A]): Schema[A] = Schema.PrimSchema(prim)

  def record[A, AP](
    fields: FreeAp2[Schema.Term[ProductTermId, A, ?], AP]
  )(
    f: A => AP,
    g: AP => A
  ): Schema[A] =
    Schema.RecordSchema(fields, f, g)

  def seq[A](element: Schema[A]): Schema[List[A]] = Schema.SeqSchema(element)

  def iso[A0, A](base: Schema[A0], iso: Iso[A0, A]): Schema[A] = Schema.IsoSchema(base, iso)

  def essentialField[A, A0](
    id: ProductTermId,
    base: Schema[A0]
  ): Schema.Term[ProductTermId, A, A0] =
    Schema.Term(id, base)

  def nonEssentialField[A, A0](
    id: ProductTermId,
    base: Schema[A0]
  ): Schema.Term[ProductTermId, A, Option[A0]] =
    Schema.Term(id, Schema.OptionalSchema(base))

  def branch[A, A0](
    id: SumTermId,
    base: Schema[A0]
  ): Schema.Term[SumTermId, A, A0] =
    Schema.Term(id, base)

  def nonEssentialBranch[A, A0](
    id: SumTermId,
    base: Schema[A0]
  ): Schema.Term[SumTermId, A, Option[A0]] =
    Schema.Term(id, Schema.OptionalSchema(base))

  sealed trait Schema[A] { self =>
    def imap[B](iso: Iso[A, B]): Schema[B] = Schema.IsoSchema(self, iso)
  }

  def union[A, AE](
    choices: FreeChoice[Schema.Term[SumTermId, A, ?], AE]
  )(f: A => AE, g: AE => A): Schema.Union[A, AE] = Schema.Union(choices, f, g)

  def optional[A, L[_] <: Schema[_]](aSchema: L[A]): Schema[Option[A]] =
    iso(
      new Schema.:+:[A, L, Unit, Schema](aSchema, Schema.Zero),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  def product[A, An, R <: Schema[An]](terms: R, representation: Iso[An, A])(
    implicit proof: Schema.LabelledProduct[R]
  ): Schema[A] =
    iso[An, A](terms, representation)

  object Schema {

    type HIso[F[_], A, B] = Iso[F[A], F[B]]



    trait LabelledProduct[T]

    implicit def labelledProduct[A, B, R[_] <: Schema[_]](
      implicit proof: LabelledProduct[R[B]]
    ): LabelledProduct[:*:[A, ProductTerm, B, R]] =
      new LabelledProduct[:*:[A, ProductTerm, B, R]] {}

    implicit def singleLabelledProduct[A]: LabelledProduct[ProductTerm[A]] =
      new LabelledProduct[ProductTerm[A]] {}

    sealed case class SumTerm[A](id: SumTermId, schema: Schema[A]) extends Schema[A]

    sealed case class ProductTerm[A](id: ProductTermId, schema: Schema[A]) extends Schema[A]

    sealed class :+:[A, L[_] <: Schema[_], B, R[_] <: Schema[_]](l: L[A], r: => R[B])
        extends Schema[A \/ B] {
      lazy val left: L[A]  = l
      lazy val right: R[B] = r
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
    }

    object One extends Schema[Unit]

    implicit class SchemaExtensions[A, R[_] <: Schema[_]](schema: R[A]) {
      def :*: [B, L[_] <: Schema[_]](left: L[B]): :*:[B, L, A, R] = new :*:(left, schema)
      def :+: [B, L[_] <: Schema[_]](left: L[B]): :+:[B, L, A, R] = new :+:(left, schema)
    }

    // Writing final here triggers a warning, using sealed instead achieves almost the same effect
    // without warning. See https://issues.scala-lang.org/browse/SI-4440
    sealed case class OptionalSchema[A](base: Schema[A]) extends Schema[Option[A]]
    sealed case class PrimSchema[A](prim: Prim[A])       extends Schema[A]
    sealed case class Union[A, AE](
      choices: FreeChoice[Term[SumTermId, A, ?], AE],
      f: A => AE,
      g: AE => A
    ) extends Schema[A]
    sealed case class RecordSchema[A, AP](
      fields: FreeAp2[Term[ProductTermId, A, ?], AP],
      f: A => AP,
      g: AP => A
    ) extends Schema[A]
    sealed case class SeqSchema[A](element: Schema[A]) extends Schema[List[A]]
    sealed case class IsoSchema[A0, A](base: Schema[A0], iso: Iso[A0, A]) extends Schema[A] {
      override def imap[B](_iso: Iso[A, B]): Schema[B] =
        IsoSchema(base, iso.composeIso(_iso))
    }

    /**
     * A term of type `A0` in a sum of type `A`.
     * For example, the `Left` term of the `Either` sum.
     */
    sealed case class Term[ID, A, A0](id: ID, base: Schema[A0])

    def covariantFold[F[_]](prims: Prim ~> F)(implicit F: Alt[F]): Schema ~> F =
      new (Schema ~> F) {

        def apply[A](schema: Schema[A]): F[A] = schema match {
          case t: PrimSchema[a] => prims(t.prim)
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
          case ProductTerm(id, base) => apply(base)
        }
      }

    def contravariantFold[F[_]](prims: Prim ~> F)(implicit F: Decidable[F]): Schema ~> F =
      new (Schema ~> F) {

        def apply[A](schema: Schema[A]): F[A] = schema match {
          case t: PrimSchema[a] => prims(t.prim)
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
          case ProductTerm(id, base) => apply(base)
        }
      }
  }

}
