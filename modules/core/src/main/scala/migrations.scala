package scalaz

package schema

import recursion._

import SchemaF._

  import monocle.Iso

sealed trait MigrationStep[Prim[_], SumTermId, ProductTermId] {

  private def addField[A, B](base: Schema[B], default: A) = Iso[B, (A, B)](b => (default, b))(_._2)

  def algebra: HAlgebra[SchemaF[Prim, SumTermId, ProductTermId, ?[_], ?], FSchema[
    Prim,
    SumTermId,
    ProductTermId,
    ?
  ]]

  def coalgebra: HCoalgebra[SchemaF[Prim, SumTermId, ProductTermId, ?[_], ?], FSchema[
    Prim,
    SumTermId,
    ProductTermId,
    ?
  ]]
}

final case class AddField[A, Prim[_], SumTermId, ProductTermId](
  name: ProductTermId,
  schema: FSchema[Prim, SumTermId, ProductTermId, A],
  default: A
) extends MigrationStep[Prim, SumTermId, ProductTermId] {

  override val algebra   = ???
  override val coalgebra = ???
}

trait HasMigrations[R <: Realisation] extends SchemaModule[R] {

  import Scalaz._



  final class UpgradingSchema(step: MigrationStep[R.Prim, R.SumTermId, R.ProductTermId]) {
    type Err = String

    def to[A](current: Schema[A]): Schema[A] = hyloNT(step.coalgebra, step.algebra).apply(current)
  }

  object Schema {

    def upgradingVia(step: MigrationStep[R.Prim, R.SumTermId, R.ProductTermId]): UpgradingSchema =
      new UpgradingSchema(step)
  }

}
