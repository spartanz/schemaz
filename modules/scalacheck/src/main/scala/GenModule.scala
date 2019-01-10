package scalaz

package schema

package scalacheck

import org.scalacheck._

import org.scalacheck._

trait GenModule[R <: Realisation] extends SchemaModule[R] {

  import SchemaF._

  implicit final def algebra(
    implicit primNT: R.Prim ~> Gen
  ): RInterpreter[Gen] =
    new Interpreter[R.Prim, R.SumTermId, R.ProductTermId, Gen] {
      val alg: HAlgebra[RSchema, Gen] = new (RSchema[Gen, ?] ~> Gen) {

      def apply[A](schema: RSchema[Gen, A]): Gen[A] =
        schema match {
          case PrimSchema(prim) => primNT(prim)
          case :*:(left, right) =>
            for {
              l <- left
              r <- right
            } yield (l, r)
          case :+:(left, right)     => Gen.oneOf(left.map(-\/(_)), right.map(\/-(_)))
          case IsoSchema(base, iso) => base.map(iso.get)
          case Record(fields, iso)  => fields.map(iso.get)
          case SeqSchema(element)   => Gen.listOf(element)
          case ProductTerm(_, base) => base
          case Union(choices, iso)  => choices.map(iso.get)
          case SumTerm(_, base)     => base
          case One()                => Gen.const(())
        }
    }

      def interpret = cataNT(alg)

    }

}
