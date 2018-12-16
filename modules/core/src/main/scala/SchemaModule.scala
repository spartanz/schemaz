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

  def optional[A, L <: Schema[A]](aSchema: L): Schema[Option[A]] =
    iso(
      Schema.:+:[A, L, Unit](aSchema, Schema.Zero()),
      Iso[Either[A, Unit], Option[A]] {
        case Left(a)  => Some(a)
        case Right(_) => None
      } {
        case Some(a) => Left(a)
        case None    => Right(())
      }
    )

  def product[A, An, R](terms: Schema.Product.Aux[An, R], representation: Iso[An, A])(
    implicit proof: Schema.LabelledProduct.Aux[R, An]
  ): Schema[A] =
    iso(terms, representation)

  object Schema {

    trait LabelledProduct[T] {
      type Out
    }

    object LabelledProduct {
      type Aux[A, O] = LabelledProduct[A] { type Out = O }
    }

    implicit def labelledProduct[A, B, R <: Schema[B]](
      implicit proof: LabelledProduct.Aux[R, B]
    ): LabelledProduct.Aux[:*:[A, ProductTerm, B, R], (A, proof.Out)] =
      new LabelledProduct[:*:[A, ProductTerm, B, R]] { type Out = (A, B) }

    implicit def labelledLast[A]
      : LabelledProduct.Aux[:*:[A, ProductTerm, Unit, One.type], (A, Unit)] =
      new LabelledProduct[:*:[A, ProductTerm, Unit, One.type]] {
        type Out = (A, Unit)
      }

    sealed trait Sum[A]                                            extends Schema[A]
    sealed case class SumTerm[A](id: SumTermId, schema: Schema[A]) extends Schema[A]
    sealed trait Product[A] extends Schema[A] {
      type Repr
    }

    object Product {
      type Aux[A, R] = Product[A] { type Repr = R }
    }

    sealed case class ProductTerm[A](id: ProductTermId, schema: Schema[A]) extends Schema[A]

    sealed case class :+:[A0, L <: Schema[A0], B](head: L, tail: Sum[B]) extends Sum[Either[A0, B]]
    sealed case class Zero()                                             extends Sum[Unit]

    sealed case class :*:[A0, L[_] <: Schema[_], B, R <: Schema[B]](
      left: L[A0],
      tail: Product.Aux[B, R]
    ) extends Product[(A0, B)] {
      type Repr = :*:[A0, L, B, R]
    }

    object One extends Product[Unit] {
      type Repr = this.type

      def :*: [A, R[_] <: Schema[_]](
        other: R[A]
      ): Product.Aux[(A, Unit), :*:[A, R, Unit, One.type]] =
        new :*:[A, R, Unit, One.type](other, this)
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

  }

}
