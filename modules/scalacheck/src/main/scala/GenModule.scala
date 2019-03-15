package scalaz

package schema

package scalacheck

import org.scalacheck._

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
          case RecordF(fields, iso)      => fields.map(iso.get)
          case SeqF(element)             => Gen.listOf(element)
          case FieldF(_, base)           => base
          case UnionF(choices, iso)      => choices.map(iso.get)
          case BranchF(_, base)          => base
          case One()                     => Gen.const(())
          case ref @ SelfReference(_, _) => Gen.delay(ref.unroll)
        }

    })

}
