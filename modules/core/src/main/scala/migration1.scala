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

    def apply[RA, A, RR, AR](base: Schema[RA, A])(
      implicit t: Transform[RA, A, P, RProd[RF, AF, RR, AR], RIso[RR, AR, (AF, AR)], (AF, AR)]
    ): Schema[t.NR, A] =
      t(
        (s: Schema[RProd[RF, AF, RR, AR], (AF, AR)]) =>
          s.unFix match {
            case p: Prod[Schema, RF, RR, AF, AR] =>
              iso(p.right, monocle.Iso[AR, (AF, AR)](x => (default, x))(p => p._2))
            case _ => ???
          }
      )(base)
  }

}
