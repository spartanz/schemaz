package scalaz

package schema

import Representation._

import shapeless._

trait HasTransform[R <: Realisation] extends SchemaModule[R] {

  sealed trait Transform[RA, A, P <: HList, R0, R1, T] {
    type NR

    def apply(transformation: Schema[R0, T] => Schema[R1, T]): Schema[RA, A] => Schema[NR, A]
  }

  sealed abstract class Partial[RA, A, P <: HList, R0, T](s: Schema[RA, A], path: P)(
    implicit atP: AtPath.Aux[RA, A, P, R0, T]
  ) {
    identity(atP)

    def transform[R1, NR](trans: Schema[R0, T] => Schema[R1, T])(
      implicit t: Transform.Aux[RA, A, P, R0, R1, T, NR]
    ): Schema[t.NR, A] = Transform.apply(s, path, trans)
  }

  object Partial {

    def apply[RA, A, P <: HList, R0, T](s: Schema[RA, A], path: P)(
      implicit atP: AtPath.Aux[RA, A, P, R0, T]
    ): Partial[RA, A, P, R0, T] = new Partial(s, path)(atP) {}
  }

  object Transform extends LowPrioTransform {

    def apply[RA, A, P <: HList, R0, R1, T, NR](
      s: Schema[RA, A],
      path: P,
      trans: Schema[R0, T] => Schema[R1, T]
    )(implicit t: Aux[RA, A, P, R0, R1, T, NR]): Schema[t.NR, A] = {
      identity(path)
      t(trans)(s)
    }

    implicit def rootTransform[RA, A, P <: HNil, R1]: Aux[RA, A, P, RA, R1, A, R1] =
      new Transform[RA, A, P, RA, R1, A] {
        type NR = R1

        def apply(transformation: Schema[RA, A] => Schema[R1, A]): Schema[RA, A] => Schema[NR, A] =
          transformation
      }

  }

  trait LowPrioTransform extends LowPrioTransform1 {

    implicit def branchTransform[RA, A, N <: R.SumTermId, PT <: HList, R0, R1, T](
      implicit inner: Transform[RA, A, PT, R0, R1, T]
    ): Aux[N -+> RA, A, N :: PT, R0, R1, T, N -+> inner.NR] =
      new Transform[N -+> RA, A, N :: PT, R0, R1, T] {
        type NR = N -+> inner.NR

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[N -+> RA, A] => Schema[NR, A] = _.unFix match {
          case BranchF(id, ra) => id -+>: inner(transformation)(ra)
          case _               => ???
        }
      }

    implicit def fieldTransform[RA, A, N <: R.ProductTermId, PT <: HList, R0, R1, T](
      implicit inner: Transform[RA, A, PT, R0, R1, T]
    ): Aux[N -*> RA, A, N :: PT, R0, R1, T, N -*> inner.NR] =
      new Transform[N -*> RA, A, N :: PT, R0, R1, T] {
        type NR = N -*> inner.NR

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[N -*> RA, A] => Schema[NR, A] = _.unFix match {
          case FieldF(id, ra) => id -*>: inner(transformation)(ra)
          case _              => ???
        }
      }

    implicit def recordTransform[RA, An, A, P <: HList, R0, R1, T, IR](
      implicit inner: Transform.Aux[RA, An, P, R0, R1, T, IR],
      ir: IsRecord[IR]
    ): Aux[RRecord[RA, An, A], A, P, R0, R1, T, RRecord[inner.NR, An, A]] =
      new Transform[RRecord[RA, An, A], A, P, R0, R1, T] {
        type NR = RRecord[inner.NR, An, A]

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[RRecord[RA, An, A], A] => Schema[RRecord[inner.NR, An, A], A] = _.unFix match {
          case RecordF(base, iso) => record(inner(transformation)(base), iso)
          case _                  => ???
        }
      }

    implicit def unionTransform[RA, An, A, P <: HList, R0, R1, T, IR](
      implicit inner: Transform.Aux[RA, An, P, R0, R1, T, IR],
      ir: IsUnion[IR]
    ): Aux[RUnion[RA, An, A], A, P, R0, R1, T, RUnion[inner.NR, An, A]] =
      new Transform[RUnion[RA, An, A], A, P, R0, R1, T] {
        type NR = RUnion[inner.NR, An, A]

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[RUnion[RA, An, A], A] => Schema[RUnion[inner.NR, An, A], A] = _.unFix match {
          case UnionF(base, iso) => union(inner(transformation)(base), iso)
          case _                 => ???
        }
      }

    implicit def isoTransform[RA, An, A, P <: HList, R0, R1, T](
      implicit inner: Transform[RA, An, P, R0, R1, T]
    ): Aux[RIso[RA, An, A], A, P, R0, R1, T, RIso[inner.NR, An, A]] =
      new Transform[RIso[RA, An, A], A, P, R0, R1, T] {
        type NR = RIso[inner.NR, An, A]

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[RIso[RA, An, A], A] => Schema[RIso[inner.NR, An, A], A] = _.unFix match {
          case IsoSchemaF(base, i) => iso(inner(transformation)(base), i)
          case _                   => ???
        }
      }

    implicit def seqTransform[RA, A, P <: HList, R0, R1, T](
      implicit inner: Transform[RA, A, P, R0, R1, T]
    ): Aux[RSeq[RA, A], List[A], P, R0, R1, T, RSeq[inner.NR, A]] =
      new Transform[RSeq[RA, A], List[A], P, R0, R1, T] {
        type NR = RSeq[inner.NR, A]

        def apply(
          transformation: Schema[R0, T] => Schema[R1, T]
        ): Schema[RSeq[RA, A], List[A]] => Schema[RSeq[inner.NR, A], List[A]] = _.unFix match {
          case SeqF(elem) => seq(inner(transformation)(elem))
          case _          => ???
        }
      }

    implicit def sumLeftTransform[RL, AL, RR, AR, P <: HList, RL0, RL1, T](
      implicit left: Transform[RL, AL, P, RL0, RL1, T]
    ): Aux[RSum[RL, AL, RR, AR], AL \/ AR, P, RL0, RL1, T, RSum[left.NR, AL, RR, AR]] =
      new Transform[RSum[RL, AL, RR, AR], AL \/ AR, P, RL0, RL1, T] {
        type NR = RSum[left.NR, AL, RR, AR]

        def apply(
          transformation: Schema[RL0, T] => Schema[RL1, T]
        ): Schema[RSum[RL, AL, RR, AR], AL \/ AR] => Schema[RSum[left.NR, AL, RR, AR], AL \/ AR] =
          _.unFix match {
            case SumF(l, r) => left(transformation)(l) :+: r
            case _          => ???
          }
      }

    implicit def prodLeftTransform[RL, AL, RR, AR, P <: HList, RL0, RL1, T](
      implicit left: Transform[RL, AL, P, RL0, RL1, T]
    ): Aux[RProd[RL, AL, RR, AR], (AL, AR), P, RL0, RL1, T, RProd[left.NR, AL, RR, AR]] =
      new Transform[RProd[RL, AL, RR, AR], (AL, AR), P, RL0, RL1, T] {
        type NR = RProd[left.NR, AL, RR, AR]

        def apply(
          transformation: Schema[RL0, T] => Schema[RL1, T]
        ): Schema[RProd[RL, AL, RR, AR], (AL, AR)] => Schema[RProd[left.NR, AL, RR, AR], (AL, AR)] =
          _.unFix match {
            case ProdF(l, r) => left(transformation)(l) :*: r
            case _           => ???
          }
      }
  }

  trait LowPrioTransform1 {

    type Aux[RA, A, P <: HList, R0, R1, T, NR0] = Transform[RA, A, P, R0, R1, T] {
      type NR = NR0
    }

    implicit def sumRightTransform[RL, AL, RR, AR, P <: HList, RR0, RR1, T](
      implicit right: Transform[RR, AR, P, RR0, RR1, T]
    ): Aux[RSum[RL, AL, RR, AR], AL \/ AR, P, RR0, RR1, T, RSum[RL, AL, right.NR, AR]] =
      new Transform[RSum[RL, AL, RR, AR], AL \/ AR, P, RR0, RR1, T] {
        type NR = RSum[RL, AL, right.NR, AR]

        def apply(
          transformation: Schema[RR0, T] => Schema[RR1, T]
        ): Schema[RSum[RL, AL, RR, AR], AL \/ AR] => Schema[RSum[RL, AL, right.NR, AR], AL \/ AR] =
          _.unFix match {
            case SumF(l, r) => l :+: right(transformation)(r)
            case _          => ???
          }
      }

    implicit def prodRightTransform[RL, AL, RR, AR, P <: HList, RR0, RR1, T](
      implicit right: Transform[RR, AR, P, RR0, RR1, T]
    ): Aux[RProd[RL, AL, RR, AR], (AL, AR), P, RR0, RR1, T, RProd[RL, AL, right.NR, AR]] =
      new Transform[RProd[RL, AL, RR, AR], (AL, AR), P, RR0, RR1, T] {
        type NR = RProd[RL, AL, right.NR, AR]

        def apply(
          transformation: Schema[RR0, T] => Schema[RR1, T]
        ): Schema[RProd[RL, AL, RR, AR], (AL, AR)] => Schema[
          RProd[RL, AL, right.NR, AR],
          (AL, AR)
        ] =
          _.unFix match {
            case ProdF(l, r) => l :*: right(transformation)(r)
            case _           => ???
          }
      }
  }

}
