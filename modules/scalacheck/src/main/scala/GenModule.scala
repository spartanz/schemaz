package scalaz

package schema

package scalacheck

import org.scalacheck._

trait GenModule[R <: Realisation] extends SchemaModule[R] {

  import Schema._

  implicit final def algebra(implicit primNT: R#Prim ~> Gen): HAlgebra[Schema[R, ?[_], ?], Gen] =
    new (Schema[R, Gen, ?] ~> Gen) {

      def apply[A](schema: Schema[R, Gen, A]): Gen[A] = schema match {
        case PrimSchema(prim) => primNT(prim)
        case :*:(left, right) =>
          for {
            l <- left
            r <- right
          } yield (l, r)
        case :+:(left, right)          => Gen.oneOf(left.map(-\/(_)), right.map(\/-(_)))
        case IsoSchema(base, iso)      => base.map(iso.get)
        case RecordSchema(fields, iso) => fields.map(iso.get)
        case SeqSchema(element)        => Gen.listOf(element)
        case ProductTerm(_, base)      => base
        case Union(choices, iso)       => choices.map(iso.get)
        case SumTerm(_, base)          => base
        case One()                     => Gen.const(())
      }
    }

}
