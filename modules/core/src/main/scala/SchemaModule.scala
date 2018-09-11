package scalaz

package schema

import monocle.{ Getter, Prism }

trait SchemaModule {
  type Prim[A]
  type SumTermId
  type ProductTermId

  def prim[A](prim: Prim[A]): Schema[A] = Schema.PrimSchema(prim)

  def record[A](fields: FreeAp[Schema.Field[A, ?], A]): Schema[A] =
    Schema.RecordSchema(fields)

  def union[A](branch: Schema.Branch[A, _], branches: Schema.Branch[A, _]*): Schema[A] =
    Schema.Union(NonEmptyList.nels(branch, branches: _*))

  def seq[A](element: Schema[A]): Schema[List[A]] = Schema.SeqSchema(element)

  def essentialField[A, A0](
    id: ProductTermId,
    base: Schema[A0],
    getter: Getter[A, A0],
    default: Option[A0]
  ): FreeAp[Schema.Field[A, ?], A0] =
    FreeAp.lift[Schema.Field[A, ?], A0](
      Schema.Field.Essential[A, A0](id, base, getter, default)
    )

  def nonEssentialField[A, A0](
    id: ProductTermId,
    base: Schema[A0],
    getter: monocle.Optional[A, A0]
  ): FreeAp[Schema.Field[A, ?], Option[A0]] =
    FreeAp.lift[Schema.Field[A, ?], Option[A0]](
      Schema.Field.NonEssential(id, base, getter)
    )

  def branch[A, A0](id: SumTermId, base: Schema[A0], prism: Prism[A, A0]): Schema.Branch[A, A0] =
    Schema.Branch(id, base, prism)

  sealed trait Schema[A]

  object Schema {
    // Writing final here triggers a warning, using sealed instead achieves almost the same effect
    // without warning. See https://issues.scala-lang.org/browse/SI-4440
    sealed case class PrimSchema[A](prim: Prim[A])                    extends Schema[A]
    sealed case class Union[A](terms: NonEmptyList[Branch[A, _]])     extends Schema[A]
    sealed case class RecordSchema[A](fields: FreeAp[Field[A, ?], A]) extends Schema[A]
    sealed case class SeqSchema[A](element: Schema[A])                extends Schema[List[A]]

    /**
     * A term of type `A0` in a sum of type `A`.
     */
    sealed trait Field[A, A0]

    object Field {
      sealed case class Essential[A, A0](
        id: ProductTermId,
        base: Schema[A0],
        getter: Getter[A, A0],
        default: Option[A0]
      ) extends Field[A, A0]
      sealed case class NonEssential[A, A0](
        id: ProductTermId,
        base: Schema[A0],
        getter: monocle.Optional[A, A0]
      ) extends Field[A, Option[A0]]
    }

    /**
     * A term of type `A0` in a sum of type `A`.
     * For example, the `Left` term of the `Either` sum.
     */
    sealed case class Branch[A, A0](id: SumTermId, base: Schema[A0], prism: Prism[A, A0])
  }
}
