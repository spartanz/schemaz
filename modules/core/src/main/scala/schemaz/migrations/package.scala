package schemaz.migrations

import schemaz._
import schemaz.Representation._

trait HasMigration[R <: Realisation] extends SchemaModule[R] {

  sealed trait DoAddField[N <: realisation.FieldId, R0, A0, T, X] {
    type A1
    type R1

    def apply(in: SchemaZ.Aux[R0, A0, T], default: X): SchemaZ.Aux[R1, A1, T]
  }

  object DoAddField extends LowPrioDoAddField {

    type Aux[N <: realisation.FieldId, R0, A0, T, X, R1_, A1_] = DoAddField[N, R0, A0, T, X] {
      type R1 = R1_; type A1 = A1_
    }

    final class DoAddFieldLeft[N <: realisation.FieldId, RI, I, RR, AR, T]
        extends DoAddField[N, RProd[N -*> RI, I, RR, AR], (I, AR), T, I] {
      type A1 = AR
      type R1 = RR

      override def apply(
        in: SchemaZ.Aux[RProd[N -*> RI, I, RR, AR], (I, AR), T],
        default: I
      ): SchemaZ.Aux[RR, AR, T] = in.structure.unFix match {
        case ProdF(_, right) =>
          SchemaZ(
            NIso[AR, (I, AR)](x => (default, x), p => p._2).compose(in.representation),
            Tag[RR].apply[Schema[AR]](right)
          )
        case _ => ???
      }
    }

    implicit def doAddFieldHere[N <: realisation.FieldId, RI, I, RR, AR, T]
      : DoAddField.Aux[N, RProd[N -*> RI, I, RR, AR], (I, AR), T, I, RR, AR] =
      new DoAddFieldLeft[N, RI, I, RR, AR, T]

    final class DoAddFieldRight[N <: realisation.FieldId, RI, I, RL, AL, T]
        extends DoAddField[N, RProd[RL, AL, N -*> RI, I], (AL, I), T, I] {
      type A1 = AL
      type R1 = RL

      def apply(
        in: SchemaZ.Aux[RProd[RL, AL, N -*> RI, I], (AL, I), T],
        default: I
      ): SchemaZ.Aux[RL, AL, T] = in.structure.unFix match {
        case ProdF(left, _) =>
          SchemaZ(
            NIso[AL, (AL, I)](x => (x, default), p => p._1).compose(in.representation),
            Tag[RL].apply[Schema[AL]](left)
          )
        case _ => ???
      }
    }

    implicit def doAddFieldHereR[N <: realisation.FieldId, RI, I, RL, AL, T]
      : DoAddField.Aux[N, RProd[RL, AL, N -*> RI, I], (AL, I), T, I, RL, AL] =
      new DoAddFieldRight[N, RI, I, RL, AL, T]
  }

  trait LowPrioDoAddField {
    final class DoAddFieldBelow[N <: realisation.FieldId, RR, AR, RL, AL, T, X, R1b, A1b](
      implicit val below: DoAddField.Aux[N, RR, AR, AR, X, R1b, A1b]
    ) extends DoAddField[N, RProd[RL, AL, RR, AR], (AL, AR), T, X] {
      type A1 = (AL, A1b)
      type R1 = RProd[RL, AL, R1b, A1b]

      def apply(
        in: SchemaZ.Aux[RProd[RL, AL, RR, AR], (AL, AR), T],
        default: X
      ): SchemaZ.Aux[RProd[RL, AL, R1b, A1b], (AL, A1b), T] =
        in.structure.unFix match {
          case ProdF(left, right) =>
            val r = below(SchemaZ(NIso.id[AR], Tag[RR].apply[Schema[AR]](right)), default)
            iso(SchemaZ(NIso.id[AL], Tag[RL].apply[Schema[AL]](left)) :*: r, in.representation)
          case _ => ???
        }
    }

    implicit def doAddFieldBelow[N <: realisation.FieldId, RR, AR, RL, AL, T, X, R1b, A1b](
      implicit below: DoAddField.Aux[N, RR, AR, AR, X, R1b, A1b]
    ): DoAddField.Aux[N, RProd[RL, AL, RR, AR], (AL, AR), T, X, RProd[RL, AL, R1b, A1b], (AL, A1b)] =
      new DoAddFieldBelow[N, RR, AR, RL, AL, T, X, R1b, A1b]

    final class DoAddFieldLast[N <: realisation.FieldId, RI]
        extends DoAddField[N, N -*> RI, RI, RI, RI] {
      type R1 = Unit
      type A1 = Unit

      def apply(
        in: SchemaZ.Aux[N -*> RI, RI, RI],
        default: RI
      ): SchemaZ.Aux[Unit, Unit, RI] =
        SchemaZ(NIso[Unit, RI](_ => default, _ => ()).compose(in.representation), unit.structure)
    }

    implicit def doAddFieldLast[N <: realisation.FieldId, RI]: DoAddField[N, N -*> RI, RI, RI, RI] =
      new DoAddFieldLast[N, RI]
  }

  implicit class MigrationRecordOps[R0: IsRecord, A0, T](
    rec: SchemaZ.Aux[RRecord[R0, A0], A0, T]
  ) {

    def addField[N <: realisation.FieldId, X, R1, A1](name: N, default: X)(
      implicit transfo: DoAddField.Aux[N, R0, A0, T, X, R1, A1]
    ): SchemaZ.Aux[RRecord[R1, A1], A1, T] =
      rec.structure.unFix match {
        case RecordF(fields) =>
          val f =
            transfo(
              SchemaZ(rec.representation, Tag[R0].apply[Schema[A0]](fields)),
              default
            )
          SchemaZ(f.representation, record(f)(new IsRecord[transfo.R1] {}).structure)
        case _ => identity(name); ???
      }

  }

}
