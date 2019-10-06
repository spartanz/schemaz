package schemaz

import Representation._

trait HasMigration[R <: Realisation] extends SchemaModule[R] {

  sealed trait DoAddField[N <: R.ProductTermId, R0, A0, T, X] {
    type A1
    type R1

    def apply(in: SchemaZ.Aux[R0, A0, T], default: X): SchemaZ.Aux[R1, A1, T]
  }

  object DoAddField extends LowPrioDoAddField {

    final class DoAddFieldLeft[N <: R.ProductTermId, RI, I, RR, AR, T]
        extends DoAddField[N, RProd[N -*> RI, I, RR, AR], (I, AR), T, I] {
      type A1 = AR
      type R1 = RR

      override def apply(
        in: SchemaZ.Aux[RProd[N -*> RI, I, RR, AR], (I, AR), T],
        default: I
      ): SchemaZ.Aux[RR, AR, T] = in.schema.unFix match {
        case ProdF(_, right) =>
          SchemaZ(
            NIso[AR, (I, AR)](x => (default, x), p => p._2).compose(in.p),
            Tag[RR].apply[Schema[AR]](right)
          )
        case _ => ???
      }
    }

    implicit def doAddFieldHere[N <: R.ProductTermId, RI, I, RR, AR, T]
      : DoAddField[N, RProd[N -*> RI, I, RR, AR], (I, AR), T, I] =
      new DoAddFieldLeft[N, RI, I, RR, AR, T]

    final class DoAddFieldRight[N <: R.ProductTermId, RI, I, RL, AL, T]
        extends DoAddField[N, RProd[RL, AL, N -*> RI, I], (AL, I), T, I] {
      type A1 = AL
      type R1 = RL

      def apply(
        in: SchemaZ.Aux[RProd[RL, AL, N -*> RI, I], (AL, I), T],
        default: I
      ): SchemaZ.Aux[RL, AL, T] = in.schema.unFix match {
        case ProdF(left, _) =>
          SchemaZ(
            NIso[AL, (AL, I)](x => (x, default), p => p._1).compose(in.p),
            Tag[RL].apply[Schema[AL]](left)
          )
        case _ => ???
      }
    }

    implicit def doAddFieldHereR[N <: R.ProductTermId, RI, I, RL, AL, T]
      : DoAddField[N, RProd[RL, AL, N -*> RI, I], (AL, I), T, I] =
      new DoAddFieldRight[N, RI, I, RL, AL, T]
  }

  trait LowPrioDoAddField {
    final class DoAddFieldBelow[N <: R.ProductTermId, RR, AR, RL, AL, T, X](
      implicit val below: DoAddField[N, RR, AR, AR, X]
    ) extends DoAddField[N, RProd[RL, AL, RR, AR], (AL, AR), T, X] {
      type A1 = (AL, below.A1)
      type R1 = RProd[RL, AL, below.R1, below.A1]

      def apply(
        in: SchemaZ.Aux[RProd[RL, AL, RR, AR], (AL, AR), T],
        default: X
      ): SchemaZ.Aux[RProd[RL, AL, below.R1, below.A1], (AL, below.A1), T] =
        in.schema.unFix match {
          case ProdF(left, right) =>
            val r = below(SchemaZ(NIso.id[AR], Tag[RR].apply[Schema[AR]](right)), default)
            iso(SchemaZ(NIso.id[AL], Tag[RL].apply[Schema[AL]](left)) :*: r, in.p)
          case _ => ???
        }
    }

    implicit def doAddFieldBelow[N <: R.ProductTermId, RR, AR, RL, AL, T, X](
      implicit below: DoAddField[N, RR, AR, AR, X]
    ): DoAddField[N, RProd[RL, AL, RR, AR], (AL, AR), T, X] =
      new DoAddFieldBelow[N, RR, AR, RL, AL, T, X]

    final class DoAddFieldLast[N <: R.ProductTermId, RI]
        extends DoAddField[N, N -*> RI, RI, RI, RI] {
      type R1 = Unit
      type A1 = Unit

      def apply(
        in: SchemaZ.Aux[N -*> RI, RI, RI],
        default: RI
      ): SchemaZ.Aux[Unit, Unit, RI] =
        SchemaZ(NIso[Unit, RI](_ => default, _ => ()).compose(in.p), unit.schema)
    }

    implicit def doAddFieldLast[N <: R.ProductTermId, RI]: DoAddField[N, N -*> RI, RI, RI, RI] =
      new DoAddFieldLast[N, RI]
  }

  implicit class MigrationRecordOps[Rn: IsRecord, An, A](
    rec: SchemaZ.Aux[RRecord[Rn, An], An, A]
  ) {

    def addField[N <: R.ProductTermId, X](name: N, default: X)(
      implicit transfo: DoAddField[N, Rn, An, An, X]
    ): SchemaZ.Aux[RRecord[transfo.R1, An], transfo.A1, A] = rec.schema.unFix match {
      case RecordF(fields) =>
        val f = transfo(SchemaZ(NIso.id[An], Tag[Rn].apply[Schema[An]](fields)), default)
        SchemaZ(f.p.compose(rec.p), record(f)(new IsRecord[transfo.R1] {}).schema)
      case _ => identity(name); ???
    }
  }

}
