package scalaz

package schema

package tests

import org.scalacheck._
import generic.GenericSchemaModule

trait GenericGenModule[R <: Realisation] extends GenericSchemaModule[R] {

  import Schema._

  implicit val genApplicativeInstance: Applicative[Gen] = new Applicative[Gen] {
    override def ap[T, U](fa: => Gen[T])(f: => Gen[T => U]): Gen[U] =
      fa.flatMap(a => f.map(_(a)))
    override def point[T](a: => T): Gen[T] = Gen.const(a)
  }

  implicit val genAltInstance: Alt[Gen] = new Alt[Gen] {
    override def point[A](a: => A): org.scalacheck.Gen[A] = Gen.const(a)
    override def ap[A, B](fa: => org.scalacheck.Gen[A])(
      f: => org.scalacheck.Gen[A => B]
    ): org.scalacheck.Gen[B] = genApplicativeInstance.ap(fa)(f)
    override def alt[X](fa: => Gen[X], fb: => Gen[X]): Gen[X] =
      for {
        a <- fa
        b <- fb
        x <- Gen.oneOf(a, b)
      } yield x
  }

  implicit final def algebra(
    implicit primNT: R.Prim ~> Gen
  ): HAlgebra[Schema[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], Gen] =
    covariantTargetFunctor[Gen](
      primNT,
      λ[Gen ~> λ[X => Gen[List[X]]]](x => Gen.listOf(x)),
      λ[RProductTerm[Gen, ?] ~> Gen](gen => gen.schema),
      λ[RSumTerm[Gen, ?] ~> Gen](gen => gen.schema),
      Gen.const(())
    )

}
