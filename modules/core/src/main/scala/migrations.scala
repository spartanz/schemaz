package scalaz

package schema

sealed trait MigrationStep

final case class AddField[A, ProductTermId, Schema[_]](
  name: ProductTermId,
  schema: Schema[A],
  default: A
) extends MigrationStep

class UpgradingSchema[Schema[_]](step: MigrationStep) {
  println(step)
  def to[A](current: Schema[A]): Schema[A] = ???
}

trait HasMigrations[R <: Realisation] extends SchemaModule[R] {

  object Schema {
    def upgradingVia(step: MigrationStep): UpgradingSchema[Schema] = ???
  }

}
