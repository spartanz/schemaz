package scalaz

package schema

import Representation._

import shapeless._

final case class Parent[T](t: T)

trait HasTransform[R <: Realisation] extends SchemaModule[R] {

  sealed trait Transform[RA, A, P <: HList, R1] {
    type NR
    type R0
    type T

    def apply(transformation: Schema[R0, T] => Schema[R1, T]): Schema[RA, A] => Schema[NR, A]
  }

  object Transform extends LowPrioTransform {

    def apply[RA, A, P <: HList, R0, R1, T, NR](
      s: Schema[RA, A],
      path: P,
      trans: Schema[R0, T] => Schema[R1, T]
    )(implicit t: Aux[RA, A, P, R0, R1, T, NR]): Schema[NR, A] = {
      identity(path)
      t(trans)(s)
    }

    implicit def rootTransform[RA, A, P <: HNil, R1]: Aux[RA, A, P, RA, R1, A, R1] =
      new Transform[RA, A, P, R1] {
        override type NR = R1
        override type R0 = RA
        override type T  = A

        def apply(transformation: Schema[RA, A] => Schema[R1, A]): Schema[RA, A] => Schema[NR, A] =
          transformation
      }

  }

  trait LowPrioTransform extends LowPrioTransform0 {
    implicit def prodLeftParentTransform[RL, N <: R.ProductTermId, RR, AR, RL1, IT]: Aux[RProd[
      N -*> RL,
      IT,
      RR,
      AR
    ], (IT, AR), Parent[N] :: HNil, RProd[N -*> RL, IT, RR, AR], RL1, (IT, AR), RL1] =
      new Transform[RProd[N -*> RL, IT, RR, AR], (IT, AR), Parent[N] :: HNil, RL1] {
        override type NR = RL1
        override type R0 = RProd[N -*> RL, IT, RR, AR]
        override type T  = (IT, AR)

        def apply(
          transformation: Schema[RProd[N -*> RL, IT, RR, AR], (IT, AR)] => Schema[RL1, (IT, AR)]
        ): Schema[RProd[N -*> RL, IT, RR, AR], (IT, AR)] => Schema[RL1, (IT, AR)] = transformation
      }

  }

  trait LowPrioTransform0 extends LowPrioTransform1 {

    implicit def branchTransform[RA, A, N <: R.SumTermId, PT <: HList, IR0, R1, IT, INR](
      implicit inner: Transform.Aux[RA, A, PT, IR0, R1, IT, INR]
    ): Aux[N -+> RA, A, N :: PT, IR0, R1, IT, N -+> INR] =
      new Transform[N -+> RA, A, N :: PT, R1] {
        override type NR = N -+> INR
        override type R0 = IR0
        override type T  = IT

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[N -+> RA, A] => Schema[NR, A] = _.unFix match {
          case BranchF(id, ra) => id -+>: (inner(transformation)(ra))
          case _               => ???
        }
      }

    implicit def fieldTransform[RA, A, N <: R.ProductTermId, PT <: HList, IR0, R1, IT, INR](
      implicit inner: Transform.Aux[RA, A, PT, IR0, R1, IT, INR]
    ): Aux[N -*> RA, A, N :: PT, IR0, R1, IT, N -*> INR] =
      new Transform[N -*> RA, A, N :: PT, R1] {
        override type NR = N -*> INR
        override type R0 = IR0
        override type T  = IT

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[N -*> RA, A] => Schema[NR, A] = _.unFix match {
          case FieldF(id, ra) => id -*>: inner(transformation)(ra)
          case _              => ???
        }
      }

    implicit def recordTransform[RA, An, A, P <: HList, IR0, R1, IT, INR](
      implicit inner: Transform.Aux[RA, An, P, IR0, R1, IT, INR],
      ir: IsRecord[INR]
    ): Aux[RRecord[RA, An, A], A, P, IR0, R1, IT, RRecord[INR, An, A]] =
      new Transform[RRecord[RA, An, A], A, P, R1] {
        override type NR = RRecord[inner.NR, An, A]
        override type R0 = IR0
        override type T  = IT

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[RRecord[RA, An, A], A] => Schema[RRecord[INR, An, A], A] = _.unFix match {
          case RecordF(base, iso) => record(inner(transformation)(base), iso)
          case _                  => ???
        }
      }

    implicit def unionTransform[RA, An, A, P <: HList, IR0, R1, IT, INR](
      implicit inner: Transform.Aux[RA, An, P, IR0, R1, IT, INR],
      ir: IsUnion[INR]
    ): Aux[RUnion[RA, An, A], A, P, IR0, R1, IT, RUnion[INR, An, A]] =
      new Transform[RUnion[RA, An, A], A, P, R1] {
        override type NR = RUnion[INR, An, A]
        override type R0 = IR0
        override type T  = IT

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[RUnion[RA, An, A], A] => Schema[RUnion[INR, An, A], A] = _.unFix match {
          case UnionF(base, iso) => union(inner(transformation)(base), iso)
          case _                 => ???
        }
      }

    implicit def isoTransform[RA, An, A, P <: HList, IR0, R1, IT, INR](
      implicit inner: Transform.Aux[RA, An, P, IR0, R1, IT, INR]
    ): Aux[RIso[RA, An, A], A, P, IR0, R1, IT, RIso[INR, An, A]] =
      new Transform[RIso[RA, An, A], A, P, R1] {
        override type NR = RIso[INR, An, A]
        override type R0 = IR0
        override type T  = IT

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[RIso[RA, An, A], A] => Schema[RIso[INR, An, A], A] = _.unFix match {
          case IsoSchemaF(base, i) => iso(inner(transformation)(base), i)
          case _                   => ???
        }
      }

    implicit def seqTransform[RA, A, P <: HList, IR0, R1, IT, INR](
      implicit inner: Transform.Aux[RA, A, P, IR0, R1, IT, INR]
    ): Aux[RSeq[RA, A], List[A], P, IR0, R1, IT, RSeq[INR, A]] =
      new Transform[RSeq[RA, A], List[A], P, R1] {
        override type NR = RSeq[INR, A]
        override type R0 = IR0
        override type T  = IT

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[RSeq[RA, A], List[A]] => Schema[RSeq[INR, A], List[A]] = _.unFix match {
          case SeqF(elem) => seq(inner(transformation)(elem))
          case _          => ???
        }
      }

    implicit def sumLeftTransform[RL, AL, RR, AR, P <: HList, RL0, RL1, IT, INR](
      implicit left: Transform.Aux[RL, AL, P, RL0, RL1, IT, INR]
    ): Aux[RSum[RL, AL, RR, AR], AL \/ AR, P, RL0, RL1, IT, RSum[INR, AL, RR, AR]] =
      new Transform[RSum[RL, AL, RR, AR], AL \/ AR, P, RL1] {
        override type NR = RSum[INR, AL, RR, AR]
        override type R0 = RL0
        override type T  = IT

        def apply(
          transformation: Schema[RL0, T] => Schema[RL1, T]
        ): Schema[RSum[RL, AL, RR, AR], AL \/ AR] => Schema[RSum[INR, AL, RR, AR], AL \/ AR] =
          _.unFix match {
            case SumF(l, r) => left(transformation)(l) :+: r
            case _          => ???
          }
      }

    implicit def prodLeftTransform[RL, AL, RR, AR, P <: HList, RL0, RL1, IT, INR](
      implicit left: Transform.Aux[RL, AL, P, RL0, RL1, IT, INR]
    ): Aux[RProd[RL, AL, RR, AR], (AL, AR), P, RL0, RL1, IT, RProd[INR, AL, RR, AR]] =
      new Transform[RProd[RL, AL, RR, AR], (AL, AR), P, RL1] {
        override type NR = RProd[INR, AL, RR, AR]
        override type R0 = RL0
        override type T  = IT

        def apply(
          transformation: Schema[RL0, T] => Schema[RL1, T]
        ): Schema[RProd[RL, AL, RR, AR], (AL, AR)] => Schema[RProd[INR, AL, RR, AR], (AL, AR)] =
          _.unFix match {
            case ProdF(l, r) => left(transformation)(l) :*: r
            case _           => ???
          }
      }
  }

  trait LowPrioTransform1 {

    type Aux[RA, A, P <: HList, R00, R1, T0, NR0] = Transform[RA, A, P, R1] {
      type NR = NR0
      type R0 = R00
      type T  = T0
    }

    implicit def sumRightTransform[RL, AL, RR, AR, P <: HList, RR0, RR1, IT, INR](
      implicit right: Transform.Aux[RR, AR, P, RR0, RR1, IT, INR]
    ): Aux[RSum[RL, AL, RR, AR], AL \/ AR, P, RR0, RR1, IT, RSum[RL, AL, INR, AR]] =
      new Transform[RSum[RL, AL, RR, AR], AL \/ AR, P, RR1] {
        override type NR = RSum[RL, AL, INR, AR]
        override type R0 = RR0
        override type T  = IT

        def apply(
          transformation: Schema[RR0, T] => Schema[RR1, T]
        ): Schema[RSum[RL, AL, RR, AR], AL \/ AR] => Schema[RSum[RL, AL, INR, AR], AL \/ AR] =
          _.unFix match {
            case SumF(l, r) => l :+: right(transformation)(r)
            case _          => ???
          }
      }

    implicit def prodRightTransform[RL, AL, RR, AR, P <: HList, RR0, RR1, IT, INR](
      implicit right: Transform.Aux[RR, AR, P, RR0, RR1, IT, INR]
    ): Aux[RProd[RL, AL, RR, AR], (AL, AR), P, RR0, RR1, IT, RProd[RL, AL, INR, AR]] =
      new Transform[RProd[RL, AL, RR, AR], (AL, AR), P, RR1] {
        override type NR = RProd[RL, AL, INR, AR]
        override type R0 = RR0
        override type T  = IT

        def apply(
          transformation: Schema[RR0, T] => Schema[RR1, T]
        ): Schema[RProd[RL, AL, RR, AR], (AL, AR)] => Schema[
          RProd[RL, AL, INR, AR],
          (AL, AR)
        ] =
          _.unFix match {
            case ProdF(l, r) => l :*: right(transformation)(r)
            case _           => ???
          }
      }
  }

}
