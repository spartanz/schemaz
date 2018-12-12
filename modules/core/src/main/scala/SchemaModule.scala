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

  object Schema {
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
