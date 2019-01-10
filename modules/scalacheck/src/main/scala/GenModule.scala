package scalaz

package schema

package scalacheck

import org.scalacheck._
import generic.GenericAlgebra

trait GenModule[R <: Realisation] extends GenericAlgebra[R] {

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

  private val seqNT: Gen ~> λ[X => Gen[List[X]]] = new (Gen ~> λ[X => Gen[List[X]]]) {
    override def apply[A](a: Gen[A]): Gen[List[A]] = Gen.listOf(a)
  }

  private def discardLabel[L]: λ[X => Gen[(L, X)]] ~> Gen = new (λ[X => Gen[(L, X)]] ~> Gen) {
    override def apply[A](a: Gen[(L, A)]): Gen[A] = a.map(_._2)
  }

  implicit final def algebra(
    implicit primNT: R.Prim ~> Gen
  ): HAlgebra[Schema[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], Gen] =
    covariantTargetFunctor[Gen](
      primNT,
      seqNT,
      discardLabel,
      discardLabel,
      Gen.const(())
    )

}
