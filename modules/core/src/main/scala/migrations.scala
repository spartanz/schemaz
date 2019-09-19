package schemaz

import Representation._

trait HasMigration[R <: Realisation] extends SchemaModule[R] {

  sealed trait DoAddField[N <: R.ProductTermId, RIn, I, X] {
    type AOut
    type ROut

    def apply[A](in: SchemaZ[NIso, RIn, A, I], default: X): SchemaZ[NIso, ROut, AOut, I]
  }

  object DoAddField extends LowPrioDoAddField {

    final class DoAddFieldLeft[N <: R.ProductTermId, RI, I, RR, AR]
        extends DoAddField[N, RProd[N -*> RI, I, RR, AR], (I, AR), I] {
      type AOut = AR
      type ROut = RR

      def apply[A](
        in: SchemaZ[NIso, RProd[N -*> RI, I, RR, AR], (A, AR), (I, AR)],
        default: I
      ): SchemaZ[NIso, RR, AR, (I, AR)] = in.schema.unFix match {
        case ProdF(_, right) =>
          SchemaZ(NIso[AR, (I, AR)](x => (default, x), p => p._2), Tag[RR].apply[Schema[AR]](right))
        case _ => ???
      }
    }

    implicit def doAddFieldHere[N <: R.ProductTermId, RI, I, RR, AR] =
      new DoAddFieldLeft[N, RI, I, RR, AR]

    final class DoAddFieldRight[N <: R.ProductTermId, RI, I, RL, AL]
        extends DoAddField[N, RProd[RL, AL, N -*> RI, I], (AL, I), I] {
      type AOut = AL
      type ROut = RL

      def apply[A](
        in: SchemaZ[NIso, RProd[RL, AL, N -*> RI, I], (AL, A), (AL, I)],
        default: I
      ): SchemaZ[NIso, RL, AL, (AL, I)] = in.schema.unFix match {
        case ProdF(left, _) =>
          SchemaZ(NIso[AL, (AL, I)](x => (x, default), p => p._1), Tag[RL].apply[Schema[AL]](left))
        case _ => ???
      }
    }

    implicit def doAddFieldHereR[N <: R.ProductTermId, RI, I, RL, AL] =
      new DoAddFieldRight[N, RI, I, RL, AL]
  }

  trait LowPrioDoAddField {
    final class DoAddFieldBelow[N <: R.ProductTermId, RR, AR, RI, AI, X](
      implicit val below: DoAddField[N, RI, AI, X]
    ) extends DoAddField[N, RProd[RR, AR, RI, AI], (AR, AI), X] {
      type AOut = Nothing
      type ROut = RProd[RR, AR, below.ROut, AI]

      def apply[P[_, _], A](
        in: SchemaZ[NIso, RProd[RR, AR, RI, AI], (AR, AI)],
        default: X
      ): SchemaZ[NIso, RProd[RR, AR, below.ROut, AI], (AR, below.AOut), (AR, AI)] = in.unFix match {
        case ProdF(left, right) => SchemaZ[RR, AR](left) :*: below(SchemaZ(right), default)
        case _                  => ???
      }
    }

    implicit def doAddFieldBelow[N <: R.ProductTermId, RR, AR, RI, AI, X](
      implicit below: DoAddField[N, RI, AI, X]
    ) = new DoAddFieldBelow[N, RR, AR, RI, AI, X]

    implicit def doAddFieldBelowI[N <: R.ProductTermId, RI, AI, I, X](
      implicit below: DoAddField[N, RI, AI, X]
    ) = new DoAddField[N, RIso[RI, AI, I], I, X] {
      type ROut = RIso[below.ROut, AI, I]

      def apply[P[_, _], A](
        in: SchemaZ[RIso[RI, AI, I], I],
        default: X
      ): SchemaZ[RIso[below.ROut, AI, I], I] =
        in.unFix match {
          case IsoSchemaF(base, isoA) =>
            iso(below(base.asInstanceOf[SchemaZ[RI, AI]], default), isoA.asInstanceOf[Iso[AI, I]])
          case _ => ???
        }
    }

    implicit def doAddFieldLast[N <: R.ProductTermId, RI] =
      new DoAddField[N, N -*> RI, RI, RI] {
        type ROut = RIso[Unit, Unit, RI]

        def apply[P[_, _], A](
          in: SchemaZ[N -*> RI, RI],
          default: RI
        ): SchemaZ[RIso[Unit, Unit, RI], RI] = iso(unit, Iso[Unit, RI](_ => default)(_ => ()))
      }
  }

  implicit class MigrationRecordOps[Rn: IsRecord, An, A](
    rec: SchemaZ[RIso[RRecord[Rn, An], An, A], A]
  ) {

    def addField[N <: R.ProductTermId, X](name: N, default: X)(
      implicit transfo: DoAddField[N, Rn, An, X]
    ): SchemaZ[RIso[RRecord[transfo.ROut, An], An, A], A] = rec.unFix match {
      case IsoSchemaF(recursion.Fix(RecordF(fields)), isoA) =>
        caseClass[transfo.ROut, An, A](
          transfo(fields.asInstanceOf[SchemaZ[Rn, An]], default),
          isoA.asInstanceOf[Iso[An, A]]
        )(
          new IsRecord[transfo.ROut] {}
        )
      case _ => identity(name); ???
    }
  }

}
