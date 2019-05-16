package scalaz

package schema

import Representation._
import shapeless._

trait HasMigration[R <: Realisation] extends HasTransform[R] {

  sealed trait Migration[R0, R1, T]

  final class AddField[P <: HList, RF, AF](path: P, default: AF) extends Migration[RF, Unit, AF] {

    identity(path)

    def apply[RA, A, RR, AR, INR](base: Schema[RA, A])(
      implicit t: Transform.Aux[RA, A, P, RProd[RF, AF, RR, AR], RIso[RR, AR, (AF, AR)], (AF, AR), INR]
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

  //This should help with type inference. We can still formulate the Operations as above (see AddField) but we can make them more use friendly by using AtPath and a Wrapper to get type inference to play along
  sealed abstract class MigrationAt[Repr, P <: HList, A, RX, AX] private (
    baseSchema: Schema[Repr, A],
    path: P
  )(implicit atPath: AtPath.Aux[Repr, A, P, RX, AX]) {

    identity(atPath)

    def addField[RL, AL, RR, AR, INR](default: AL)(
      implicit ev: Unpack4[RX, RProd, RL, AL, RR, AR],
      ev2: Unpack2[AX, Tuple2, AL, AR],
      t: Transform.Aux[Repr, A, P, RProd[RL, AL, RR, AR], RIso[RR, AR, (AL, AR)], (AL, AR), INR]
    ): Schema[INR, A] = {
      identity(ev)
      identity(ev2)
      new AddField(
        path,
        default
      ).apply(baseSchema)
    }
    //other migrations
  }

  object MigrationAt {

    def apply[P <: HList, Repr, A, RX, AX](baseSchema: Schema[Repr, A], path: P)(
      implicit atPath: AtPath.Aux[Repr, A, P, RX, AX]
    ) = new MigrationAt(baseSchema, path) {}
  }

}
