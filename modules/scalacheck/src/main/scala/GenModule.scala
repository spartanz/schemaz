package schemaz

package scalacheck

import org.scalacheck._
import scalaz.{ -\/, \/-, ~> }

trait GenModule[R <: Realisation] extends SchemaModule[R] {

  implicit final def genInterpreter(
    implicit primNT: R.Prim ~> Gen
  ): RInterpreter[Gen] =
    Interpreter.cata(new (RSchema[Gen, ?] ~> Gen) {

      def apply[A](schema: RSchema[Gen, A]): Gen[A] =
        schema match {
          case PrimSchemaF(prim) => primNT(prim)
          case ProdF(left, right) =>
            for {
              l <- left
              r <- right
            } yield (l, r)
          case SumF(left, right)         => Gen.oneOf(left.map(-\/(_)), right.map(\/-(_)))
          case IsoSchemaF(base, iso)     => base.map(iso.get)
          case RecordF(fields)           => fields
          case SeqF(element)             => Gen.listOf(element)
          case FieldF(_, base)           => base
          case UnionF(choices)           => choices
          case BranchF(_, base)          => base
          case One()                     => Gen.const(())
          case ref @ SelfReference(_, _) => Gen.delay(ref.unroll)
        }

    })

}
