package scalaz

package schema

import Representation._
import shapeless.HList

trait HasMigration[R <: Realisation] extends HasTransform[R] {

  sealed trait Migration[R0, R1, T]

  final class AddField[P <: HList, RF, AF](path: P, field: Schema[RF, AF], default: AF)
      extends Migration[RF, Unit, AF] {

    identity(path)
    identity(field)

    def apply[RA, A](base: Schema[RA, A])(
      implicit t: Transform[RA, A, P, RF, RIso[Unit, Unit, AF], AF]
    ): Schema[t.NR, A] =
      t((_: Schema[RF, AF]) => iso(unit, monocle.Iso[Unit, AF](_ => default)(_ => ())))(base)
  }

  

}
