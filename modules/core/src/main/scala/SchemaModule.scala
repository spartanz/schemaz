package schemaz
import scala.annotation.implicitNotFound

import recursion._

import monocle.Iso
import scalaz.{ -\/, \/, \/-, ~> }

trait Realisation {
  type Prim[A]
  type SumTermId
  type ProductTermId
}

object Representation {
  type RSum[RA, A, RB, B]
  type RProd[RA, A, RB, B]
  type RIso[RA, A, B]
  type RSelf[A]
  type RSeq[R, A]
  type -*>[K, V]
  type -+>[K, V]
  type RRecord[RA, An, A]
  type RUnion[RA, An, A]
}

import Representation._

sealed trait SchemaF[Prim[_], SumTermId, ProductTermId, F[_], A] {
  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A]
}

trait SelfRef {}

////////////////////
// The Schema ADT
////////////////////

// "Essential" nodes. In theory every possible type can be represented using only `One`, `:+:` and `:*:`

final case class One[F[_], Prim[_], SumTermId, ProductTermId]()
    extends SchemaF[Prim, SumTermId, ProductTermId, F, Unit] {
  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, Unit] = One()
}

/**
 * The sum of two schemas, yielding the schema for `A \/ B`
 */
final case class SumF[F[_], A, B, Prim[_], SumTermId, ProductTermId](
  left: F[A],
  right: F[B]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A \/ B] {

  def hmap[G[_]](
    nt: F ~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, A \/ B] =
    SumF(
      nt(left),
      nt(right)
    )
  override def toString: String = s"$left :+: $right"
}

/**
 * The product of two schemas, yielding the schema for `(A, B)`
 */
final case class ProdF[F[_], A, B, Prim[_], SumTermId, ProductTermId](
  left: F[A],
  right: F[B]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, (A, B)] {

  def hmap[G[_]](
    nt: F ~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, (A, B)] =
    ProdF(nt(left), nt(right))
  override def toString: String = s"$left :*: $right"
}

// "Extra" nodes, making it more convenient to represent real-world types

/**
 * The schema of a primitive type in the context of this `SchemaModule`
 */
final case class PrimSchemaF[F[_], A, Prim[_], SumTermId, ProductTermId](prim: Prim[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    PrimSchemaF[G, A, Prim, SumTermId, ProductTermId](prim)
}

/**
 * A named branch of an union
 */
final case class BranchF[F[_], A, Prim[_], SumTermId, ProductTermId](
  id: SumTermId,
  schema: F[A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    BranchF(id, nt(schema))
}

/**
 * An union, eg. a sum of named branches
 * This class cannot be constructed directly, you must use the `SchemaModule#union` method.
 */
final case class UnionF[F[_], A, AE, Prim[_], SumTermId, ProductTermId](
  choices: F[AE],
  iso: Iso[AE, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    UnionF[G, A, AE, Prim, SumTermId, ProductTermId](nt(choices), iso)
}

/**
 * A named field of a record
 */
final case class FieldF[F[_], A, Prim[_], SumTermId, ProductTermId](
  id: ProductTermId,
  schema: F[A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    FieldF(id, nt(schema))
}

/**
 * A record, eg. a product of named fields
 * This class cannot be constructed directly, you must use the `SchemaModule#record` method.
 */
final case class RecordF[F[_], A, AP, Prim[_], SumTermId, ProductTermId](
  fields: F[AP],
  iso: Iso[AP, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](
    nt: F ~> G
  ): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    RecordF[G, A, AP, Prim, SumTermId, ProductTermId](nt(fields), iso)
}

/**
 * A sequence
 */
final case class SeqF[F[_], A, Prim[_], SumTermId, ProductTermId](element: F[A])
    extends SchemaF[Prim, SumTermId, ProductTermId, F, List[A]] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, List[A]] =
    SeqF(nt(element))
}

/**
 * The schema obtained by "mapping" an Iso of top of a schema. If there is an isomorphism
 * between AO and A, then a schema of A0 can be used to represent values of A.
 */
final case class IsoSchemaF[F[_], A0, A, Prim[_], SumTermId, ProductTermId](
  base: F[A0],
  iso: Iso[A0, A]
) extends SchemaF[Prim, SumTermId, ProductTermId, F, A] {

  def hmap[G[_]](nt: F ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    IsoSchemaF(nt(base), iso)
}

final case class SelfReference[F[_], H[_], A, Prim[_], SumTermId, ProductTermId](
  private val ref: () => F[A],
  private val nattrans: F ~> H
) extends SchemaF[Prim, SumTermId, ProductTermId, H, A] {

  lazy val unroll: H[A] = nattrans(ref())

  def hmap[G[_]](nt: H ~> G): SchemaF[Prim, SumTermId, ProductTermId, G, A] =
    SelfReference[F, G, A, Prim, SumTermId, ProductTermId](ref, nt.compose(nattrans))
}

/**
 * An interpreter able to derive a `F[A]` from a schema for `A` (for any `A`).
 * Such interpreters will usually be implemented using a recursion scheme like
 * 'cataNT`or hyloNT`.
 */
trait Interpreter[F[_], G[_]] { self =>

  /**
   * A natural transformation that will transform a schema for any type `A`
   * into an `F[A]`.
   */
  def interpret: F ~> G

  def compose[H[_]](nt: H ~> F) = self match {
    case i: ComposedInterpreter[h, G, F] => ComposedInterpreter(i.underlying, i.nt.compose(nt))
    case x                               => ComposedInterpreter(x, nt)
  }
}

final case class ComposedInterpreter[F[_], G[_], H[_]](
  underlying: Interpreter[F, G],
  nt: H ~> F
) extends Interpreter[H, G] {
  final override val interpret = underlying.interpret.compose(nt)
}

class CataInterpreter[S[_[_], _], F[_]](
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[Fix[S, ?], F] {
  final override val interpret = cataNT(algebra)
}

class HyloInterpreter[S[_[_], _], F[_], G[_]](
  coalgebra: HCoalgebra[S, G],
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[G, F] {
  final override val interpret = hyloNT(coalgebra, algebra)
}

@implicitNotFound(
  msg = "It seems like the following representation type isn't isomorphic to a product of named fields: ${A}"
)
trait IsRecord[A]

object IsRecord {
  implicit def singleFieldIsRecord[K, V]: IsRecord[K -*> V] = new IsRecord[K -*> V] {}

  implicit def productIsRecord[L: IsRecord, R: IsRecord, X, Y]: IsRecord[RProd[L, X, R, Y]] =
    new IsRecord[RProd[L, X, R, Y]] {}

  implicit def isoIsRecord[R: IsRecord, A0, A]: IsRecord[RIso[R, A0, A]] =
    new IsRecord[RIso[R, A0, A]] {}
}

@implicitNotFound(
  msg = "It seems like the following representation type isn't isomorphic to a sum of named branches: ${A}"
)
trait IsUnion[A]

object IsUnion {

  implicit def singleBranchIsUnion[K, V]: IsUnion[K -+> V] = new IsUnion[K -+> V] {}

  implicit def productIsUnion[L: IsUnion, R: IsUnion, X, Y]: IsUnion[RSum[L, X, R, Y]] =
    new IsUnion[RSum[L, X, R, Y]] {}
}

object SchemaF {

  implicit def schemaHFunctor[Prim[_], SumTermId, ProductTermId] =
    new HFunctor[SchemaF[Prim, SumTermId, ProductTermId, ?[_], ?]] {

      def hmap[F[_], G[_]](nt: F ~> G) =
        new (SchemaF[Prim, SumTermId, ProductTermId, F, ?] ~> SchemaF[
          Prim,
          SumTermId,
          ProductTermId,
          G,
          ?
        ]) {
          def apply[A](fa: SchemaF[Prim, SumTermId, ProductTermId, F, A]) = fa.hmap(nt)
        }
    }

}

trait SchemaModule[R <: Realisation] {

  val R: R

  type RInterpreter[F[_]] = Interpreter[Schema, F]

  type RSchema[F[_], A] = SchemaF[R.Prim, R.SumTermId, R.ProductTermId, F, A]

  type Schema[A] =
    Fix[SchemaF[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], A]

  type SchemaZ[Repr, A]

  object SchemaZ {
    def tag[Repr, A](schema: Schema[A]): SchemaZ[Repr, A]    = schema.asInstanceOf[SchemaZ[Repr, A]]
    def untag[Repr, A](schemaz: SchemaZ[Repr, A]): Schema[A] = schemaz.asInstanceOf[Schema[A]]

    implicit def forget[Repr, A](schemaz: SchemaZ[Repr, A]): Schema[A] = untag(schemaz)
  }

  type ROne[F[_]]            = One[F, R.Prim, R.SumTermId, R.ProductTermId]
  type RPrim[F[_], A]        = PrimSchemaF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Sum[F[_], A, B]       = SumF[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Prod[F[_], A, B]      = ProdF[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Branch[F[_], A]       = BranchF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Union[F[_], AE, A]    = UnionF[F, A, AE, R.Prim, R.SumTermId, R.ProductTermId]
  type Field[F[_], A]        = FieldF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type Record[F[_], An, A]   = RecordF[F, A, An, R.Prim, R.SumTermId, R.ProductTermId]
  type Sequence[F[_], A]     = SeqF[F, A, R.Prim, R.SumTermId, R.ProductTermId]
  type IsoSchema[F[_], A, B] = IsoSchemaF[F, A, B, R.Prim, R.SumTermId, R.ProductTermId]
  type Self[F[_], A]         = SelfReference[Any, F, A, R.Prim, R.SumTermId, R.ProductTermId]

  object Interpreter {

    def cata[S[_[_], _], F[_]](alg: HAlgebra[S, F])(implicit ev: HFunctor[S]) =
      new CataInterpreter[S, F](alg)

    def hylo[S[_[_], _], F[_], G[_]](coalg: HCoalgebra[S, G], alg: HAlgebra[S, F])(
      implicit ev: HFunctor[S]
    ) = new HyloInterpreter(coalg, alg)

  }
  ////////////////
  // Public API
  ////////////////

  implicit final class SchemaSyntax[A](schema: Schema[A]) {

    def -*>: [I <: R.ProductTermId](
      id: I
    ): SchemaZ[I -*> A, A] =
      SchemaZ.tag(Fix(FieldF(id.asInstanceOf[R.ProductTermId], schema)))

    def -+>: [I <: R.SumTermId](id: I): SchemaZ[I -+> A, A] =
      SchemaZ.tag(Fix(BranchF(id.asInstanceOf[R.SumTermId], schema)))

  }

  implicit final class SchemaZSyntax[Repr, A](schema: SchemaZ[Repr, A]) {

    import SchemaZ.{ tag, untag }

    def :*: [R2, B](left: SchemaZ[R2, B]): SchemaZ[RProd[R2, B, Repr, A], (B, A)] =
      tag(Fix(new ProdF(untag(left), SchemaZ.untag(schema))))

    def :+: [R2, B](left: SchemaZ[R2, B]): SchemaZ[RSum[R2, B, Repr, A], B \/ A] =
      tag(Fix(new SumF(untag(left), untag(schema))))

    def -*>: [I <: R.ProductTermId](
      id: I
    ): SchemaZ[I -*> Repr, A] =
      tag(Fix(FieldF(id.asInstanceOf[R.ProductTermId], untag(schema))))

    def -+>: [I <: R.SumTermId](id: I): SchemaZ[I -+> Repr, A] =
      tag(Fix(BranchF(id.asInstanceOf[R.SumTermId], untag(schema))))

    def to[F[_]](implicit interpreter: RInterpreter[F]): F[A] = interpreter.interpret(untag(schema))

    def imap[B](_iso: Iso[A, B]): SchemaZ[RIso[Repr, A, B], B] =
      tag(Fix(IsoSchemaF(untag(schema), _iso)))
    /*
    schema.unFix match {
      case i: IsoSchema[Schema, Repr, a0, A] =>
        Fix(
          IsoSchemaF[Schema, Repr, a0, B, R.Prim, R.SumTermId, R.ProductTermId](
            i.base,
            i.iso.composeIso(_iso)
          )
        )
      case _ => Fix(IsoSchemaF(schema, _iso))
    }
   */
  }

  final def unit: SchemaZ[Unit, Unit] =
    SchemaZ.tag(
      Fix(
        One()
      )
    )

  final def prim[A](prim: R.Prim[A]): SchemaZ[A, A] =
    SchemaZ.tag(
      Fix(
        PrimSchemaF(prim)
      )
    )

  final def union[Repr: IsUnion, A, AE](
    choices: SchemaZ[Repr, AE],
    iso: Iso[AE, A]
  ): SchemaZ[RUnion[Repr, AE, A], A] =
    SchemaZ.tag(Fix(UnionF(SchemaZ.untag(choices), iso)))

  final def optional[A](
    aSchema: Schema[A]
  ): SchemaZ[RIso[RSum[A, A, Unit, Unit], A \/ Unit, Option[A]], Option[A]] =
    iso(
      SchemaZ.tag[RSum[A, A, Unit, Unit], A \/ Unit](
        Fix(SumF(aSchema, SchemaZ.untag(unit)))
      ),
      Iso[A \/ Unit, Option[A]](_.swap.toOption)(_.fold[A \/ Unit](\/-(()))(-\/(_)))
    )

  final def record[Repr: IsRecord, A, An](
    terms: SchemaZ[Repr, An],
    isoA: Iso[An, A]
  ): SchemaZ[RRecord[Repr, An, A], A] =
    SchemaZ.tag(Fix(RecordF(SchemaZ.untag(terms), isoA)))

  final def seq[Repr, A](element: SchemaZ[Repr, A]): SchemaZ[RSeq[Repr, A], List[A]] =
    SchemaZ.tag(Fix(SeqF(SchemaZ.untag(element))))

  final def iso[Repr, A0, A](
    base: SchemaZ[Repr, A0],
    iso: Iso[A0, A]
  ): SchemaZ[RIso[Repr, A0, A], A] =
    SchemaZ.tag(Fix(IsoSchemaF(SchemaZ.untag(base), iso)))

  final def self[A](root: => Schema[A]): SchemaZ[RSelf[A], A] =
    SchemaZ.tag(
      Fix(
        SelfReference(() => root, new (Schema ~> Schema) {
          def apply[X](a: Schema[X]) = a
        })
      )
    )

}
