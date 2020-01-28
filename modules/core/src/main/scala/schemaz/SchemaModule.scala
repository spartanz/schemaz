package schemaz

import recursion._
import scalaz.{ -\/, \/, \/-, ~> }

import Representation._

trait SchemaModule[R <: Realisation] {

  val realisation: R

  type RInterpreter[F[_]] = Interpreter[Schema, F]

  type RSchema[F[_], A] =
    SchemaF[realisation.Prim, realisation.BranchId, realisation.FieldId, F, A]

  type Schema[A] =
    Fix[SchemaF[realisation.Prim, realisation.BranchId, realisation.FieldId, ?[_], ?], A]

  trait SchemaZ[T] {
    type Repr
    type A

    def representation: NIso[A, T]
    def structure: Schema[A] with Tagged[Repr]

    def :*: [R2, B, U](
      left: SchemaZ.Aux[R2, B, U]
    )(implicit P: Distributes[NIso, Tuple2]): SchemaZ.Aux[RProd[R2, B, Repr, A], (B, A), (U, T)] =
      SchemaZ(
        P.dist(left.representation, representation),
        Tag[RProd[R2, B, Repr, A]].apply[Schema[(B, A)]](Fix(new ProdF(left.structure, structure)))
      )

    def :+: [R2, B, U](
      left: SchemaZ.Aux[R2, B, U]
    )(implicit P: Distributes[NIso, \/]): SchemaZ.Aux[RSum[R2, B, Repr, A], B \/ A, U \/ T] =
      SchemaZ(
        P.dist(left.representation, representation),
        Tag[RSum[R2, B, Repr, A]].apply[Schema[B \/ A]](Fix(new SumF(left.structure, structure)))
      )

    def to[F[_]](implicit interpreter: RInterpreter[F], trans: Transform[F]): F[T] =
      trans(interpreter.interpret(structure), representation)

  }

  object SchemaZ {

    type Aux[R0, A0, T] = SchemaZ[T] {
      type Repr = R0; type A = A0
    }

    def apply[R0, A0, T](
      representation: NIso[A0, T],
      structure: Schema[A0] with Tagged[R0]
    ): SchemaZ.Aux[R0, A0, T] = SchemaZImpl[R0, A0, T](representation, structure)
  }

  sealed private case class SchemaZImpl[R0, A0, T](
    representation: NIso[A0, T],
    structure: Schema[A0] with Tagged[R0]
  ) extends SchemaZ[T] {

    type Repr = R0
    type A    = A0

  }

  implicit class ProductTermIdOps[I <: realisation.FieldId](id: I) {

    def -*> [T](schema: SchemaZ[T]): SchemaZ.Aux[I -*> schema.Repr, schema.A, T] =
      SchemaZ(
        schema.representation,
        Tag[I -*> schema.Repr]
          .apply[Schema[schema.A]](
            Fix(FieldF(id.asInstanceOf[realisation.FieldId], schema.structure))
          )
      )
  }

  implicit class SumTermIdOps[I <: realisation.BranchId](id: I) {

    def -+> [T](schema: SchemaZ[T]): SchemaZ.Aux[I -+> schema.Repr, schema.A, T] =
      SchemaZ(
        schema.representation,
        Tag[I -+> schema.Repr]
          .apply[Schema[schema.A]](
            Fix(BranchF(id.asInstanceOf[realisation.BranchId], schema.structure))
          )
      )
  }

  type ROne[F[_]] = One[F, realisation.Prim, realisation.BranchId, realisation.FieldId]

  type RPrim[F[_], A] =
    PrimSchemaF[F, A, realisation.Prim, realisation.BranchId, realisation.FieldId]

  type Sum[F[_], A, B] =
    SumF[F, A, B, realisation.Prim, realisation.BranchId, realisation.FieldId]

  type Prod[F[_], A, B] =
    ProdF[F, A, B, realisation.Prim, realisation.BranchId, realisation.FieldId]

  type Branch[F[_], A] =
    BranchF[F, A, realisation.Prim, realisation.BranchId, realisation.FieldId]

  type Union[F[_], A] =
    UnionF[F, A, realisation.Prim, realisation.BranchId, realisation.FieldId]

  type Field[F[_], A] =
    FieldF[F, A, realisation.Prim, realisation.BranchId, realisation.FieldId]

  type Record[F[_], A] =
    RecordF[F, A, realisation.Prim, realisation.BranchId, realisation.FieldId]

  type Sequence[F[_], A] =
    SeqF[F, A, realisation.Prim, realisation.BranchId, realisation.FieldId]

  type Self[F[_], A] =
    SelfReference[Any, F, A, realisation.Prim, realisation.BranchId, realisation.FieldId]

  ////////////////
  // Public API
  ////////////////

  final def unit: SchemaZ.Aux[Unit, Unit, Unit] =
    SchemaZ(
      NIso.id,
      Tag[Unit].apply[Schema[Unit]](
        Fix(
          One()
        )
      )
    )

  final def prim[A](prim: realisation.Prim[A]): SchemaZ.Aux[A, A, A] =
    SchemaZ(
      NIso.id,
      Tag[A].apply[Schema[A]](
        Fix(
          PrimSchemaF(prim)
        )
      )
    )

  final def union[Repr: IsUnion, A, T](
    choices: SchemaZ.Aux[Repr, A, T]
  ): SchemaZ.Aux[RUnion[Repr, A], A, T] =
    SchemaZ(
      choices.representation,
      Tag[RUnion[Repr, A]].apply[Schema[A]](Fix(UnionF(choices.structure)))
    )

  final def sealedTrait[Repr: IsUnion, Branches, A, T](
    branches: SchemaZ.Aux[Repr, A, Branches],
    isoA: NIso[Branches, T]
  ): SchemaZ.Aux[RUnion[Repr, Branches], A, T] =
    SchemaZ(
      branches.representation.compose(isoA),
      Tag[RUnion[Repr, Branches]].apply[Schema[A]](union(branches).structure)
    )

  final def optional[T](
    aSchema: SchemaZ[T]
  ): SchemaZ.Aux[RSum[aSchema.Repr, aSchema.A, Unit, Unit], aSchema.A \/ Unit, Option[T]] =
    SchemaZ(
      NIso[aSchema.A \/ Unit, Option[T]](
        _.swap.toOption.map(aSchema.representation.f),
        _.fold[aSchema.A \/ Unit](\/-(()))(a0 => -\/(aSchema.representation.g(a0)))
      ),
      Tag[RSum[aSchema.Repr, aSchema.A, Unit, Unit]]
        .apply[Schema[aSchema.A \/ Unit]](Fix(SumF(aSchema.structure, unit.structure)))
    )

  final def record[Repr: IsRecord, A, T](
    terms: SchemaZ.Aux[Repr, A, T]
  ): SchemaZ.Aux[RRecord[Repr, A], A, T] =
    SchemaZ(
      terms.representation,
      Tag[RRecord[Repr, A]].apply[Schema[A]](Fix(RecordF(terms.structure)))
    )

  final def caseClass[Repr: IsRecord, Fields, A, T](
    fields: SchemaZ.Aux[Repr, A, Fields],
    isoA: NIso[Fields, T]
  ): SchemaZ.Aux[RRecord[Repr, A], A, T] =
    SchemaZ(fields.representation.compose(isoA), record(fields).structure)

  final def seq[Repr, A, T](
    element: SchemaZ.Aux[Repr, A, T]
  ): SchemaZ.Aux[RSeq[Repr, A], List[A], List[T]] =
    SchemaZ(
      NIso[List[A], List[T]](_.map(element.representation.f), _.map(element.representation.g)),
      Tag[RSeq[Repr, A]].apply[Schema[List[A]]](Fix(SeqF(element.structure)))
    )

  final def iso[A0, A](
    base: SchemaZ[A0],
    iso: NIso[A0, A]
  ): SchemaZ.Aux[base.Repr, base.A, A] =
    SchemaZ(base.representation.compose(iso), base.structure)

  final def self[A](root: => Schema[A]): SchemaZ.Aux[RSelf[A], A, A] =
    SchemaZ(
      NIso.id,
      Tag[RSelf[A]].apply[Schema[A]](
        Fix(
          SelfReference(() => root, new (Schema ~> Schema) {
            def apply[X](a: Schema[X]) = a
          })
        )
      )
    )

}
