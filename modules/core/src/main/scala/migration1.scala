package schemaz

import Representation._

import monocle.Iso

trait HasMigration[R <: Realisation] extends SchemaModule[R] {

  trait DoAddField[N <: R.ProductTermId, RIn, I, X] {
    type ROut

    def apply(in: SchemaZ[RIn, I], default: X): SchemaZ[ROut, I]
  }

  object DoAddField extends LowPrioDoAddField {

    implicit def doAddFieldHere[N <: R.ProductTermId, RI, I, RR, AR] =
      new DoAddField[N, RProd[N -*> RI, I, RR, AR], (I, AR), I] {
        type ROut = RIso[RR, AR, (I, AR)]

        def apply(
          in: SchemaZ[RProd[N -*> RI, I, RR, AR], (I, AR)],
          default: I
        ): SchemaZ[RIso[RR, AR, (I, AR)], (I, AR)] = in.unFix match {
          case ProdF(_, right) => iso(right, Iso[AR, (I, AR)](x => (default, x))(p => p._2))
          case _               => ???
        }
      }

    implicit def doAddFieldHereR[N <: R.ProductTermId, RI, I, RL, AL] =
      new DoAddField[N, RProd[RL, AL, N -*> RI, I], (AL, I), I] {
        type ROut = RIso[RL, AL, (AL, I)]

        def apply(
          in: SchemaZ[RProd[RL, AL, N -*> RI, I], (AL, I)],
          default: I
        ): SchemaZ[RIso[RL, AL, (AL, I)], (AL, I)] = in.unFix match {
          case ProdF(left, _) => iso(left, Iso[AL, (AL, I)](x => (x, default))(p => p._1))
          case _              => ???
        }
      }
  }

  trait LowPrioDoAddField {
    implicit def doAddFieldBelow[N <: R.ProductTermId, RR, AR, RI, AI, X](
      implicit below: DoAddField[N, RI, AI, X]
    ) = new DoAddField[N, RProd[RR, AR, RI, AI], (AR, AI), X] {
      type ROut = RProd[RR, AR, below.ROut, AI]

      def apply(
        in: SchemaZ[RProd[RR, AR, RI, AI], (AR, AI)],
        default: X
      ): SchemaZ[RProd[RR, AR, below.ROut, AI], (AR, AI)] = in.unFix match {
        case ProdF(left, right) => left :*: below(right, default)
        case _                  => ???
      }
    }

    implicit def doAddFieldBelowI[N <: R.ProductTermId, RI, AI, I, X](
      implicit below: DoAddField[N, RI, AI, X]
    ) = new DoAddField[N, RIso[RI, AI, I], I, X] {
      type ROut = RIso[below.ROut, AI, I]

      def apply(in: SchemaZ[RIso[RI, AI, I], I], default: X): SchemaZ[RIso[below.ROut, AI, I], I] =
        in.unFix match {
          case IsoSchemaF(base, isoA) => iso(below(base, default), isoA)
          case _                      => ???
        }
    }

    implicit def doAddFieldLast[N <: R.ProductTermId, RI] =
      new DoAddField[N, N -*> RI, RI, RI] {
        type ROut = RIso[Unit, Unit, RI]

        def apply(
          in: SchemaZ[N -*> RI, RI],
          default: RI
        ): SchemaZ[RIso[Unit, Unit, RI], RI] = iso(unit, Iso[Unit, RI](_ => default)(_ => ()))
      }
  }

  implicit class MigrationRecordOps[Rn: IsRecord, An, A](rec: SchemaZ[RRecord[Rn, An, A], A]) {

    def addField[N <: R.ProductTermId, X](name: N, default: X)(
      implicit transfo: DoAddField[N, Rn, An, X]
    ): SchemaZ[RRecord[transfo.ROut, An, A], A] = rec.unFix match {
      case RecordF(fields, isoA) =>
        record(transfo(fields, default), isoA)(new IsRecord[transfo.ROut] {})
      case _ => identity(name); ???
    }
  }
}
